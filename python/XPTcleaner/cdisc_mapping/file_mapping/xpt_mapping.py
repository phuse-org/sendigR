import pandas
import os
import json

from cdisc_mapping import utils
from cdisc_mapping import logger

pandas.options.mode.chained_assignment = None

LOG = logger.get_logger(__name__)


def check_valid_files(input_dir):
    """
    Returns true if dm.xpt is available, otherwise false
    """
    input_xpt = os.listdir(input_dir)
    input_xpt = [i for i in input_xpt if i.endswith('.xpt')]
    input_xpt_cleaned = [x.lower() for x in input_xpt]
    files_dict = dict(zip(input_xpt_cleaned, input_xpt))
    #search in files_dict.keys() for cleaned file name
    required_xpts = ["dm.xpt"]
    optional_xpts = ["mi.xpt", "ds.xpt", "ex.xpt", "ts.xpt"]
    combo_xpts = [required_xpts, optional_xpts]
  
    # Get the XPT files that will not be mapped
    xpt_noMap = [v for v in list(files_dict.keys()) if v not in set().union(*combo_xpts)]

    #[files_dict.pop(x, None) for x in xpt_noMap]
    xpt_noMap_dict = dict((d, files_dict.pop(d, None)) for d in xpt_noMap)
    required_dict = dict((d, files_dict.pop(d, None)) for d in required_xpts)

    # Logic depending if dm.xpt found or not
    if None not in required_dict.values(): #dm found
        return True, required_dict, files_dict, xpt_noMap_dict
    else: #MI not found
        return False, required_dict, files_dict, xpt_noMap_dict

def do_mapping(column, term, json_file):
    """
    Returns mapped terms from column and provided
    vocab file
    """

    with open(json_file, "r") as j:
        contents = json.loads(j.read())

    # make sure we have the column
    if column not in contents.keys():
        LOG.error("Column: " + column + "was not found in keys")
        raise ValueError("Column not in contents keys")

    # consider doing text standardization
    stan_term = utils.standardize_text(term)
    if stan_term in contents[column]:
        mapped_term = contents[column][stan_term]
        if (stan_term != mapped_term):
            LOG.info(
                '"' + term + '","' + stan_term + '","' + mapped_term+ '", ' + column
            #    ", Original Term, " + term + ", Standardized term, " + stan_term + ", Mapped term,  " + mapped_term + " ,Codelist, " + column
            )
        return contents[column][stan_term]
    elif stan_term == '':
        # Possibly warn this is blank?
        return "NA"
    else:
         LOG.warning(
            '"' + term + '","' + stan_term + '",, ' + column
         )         #return "UNMAPPED"
        # leave the raw value as it is, it is up to the user to check the log file and correct all the mapping
         return term

def MI_dataframe(xpt_dir, xpt_dict, json_file):

    mi_xpt, meta  = utils.read_XPT(xpt_dir, xpt_dict['mi.xpt'])

    # General Histopathologic Exam, Qual
    dfMI= mi_xpt[mi_xpt["MITESTCD"] == "GHISTXQL"]
    if len(dfMI) == 0:
        #This may be a 3.0 dataset with MITESTCD = "MIEXAM"
        dfMI = mi_xpt[mi_xpt["MITESTCD"] == "MIEXAM"]
    if len(dfMI) != len(mi_xpt):
        MIdiff = len(mi_xpt) - len(dfMI)
        LOG.warning(
            str(MIdiff)
            + " rows are not 'General Histopathologic Exam, Qual' or 'Microscopic Examination' and will be excluded"
        )

    # Do mapping on available columns
    columns_to_map = {
        "MISTRESC": "Non-Neoplastic Finding Type",
        "MISPEC": "Specimen",
        # Anatomical Location is not for MIANTREG, it is for FXLOC
        # "MIANTREG": "Anatomical Location",
        "MISEV": "SEND Severity"
    }

    dfMI_map = utils.column_mapping(columns_to_map, dfMI, json_file)

    return dfMI_map, meta


def EX_dataframe(xpt_dir, xpt_dict, json_file):

    # EX.xpt takes priority for route over TS.xpt
    # TODO: need flexible name strategy
    ex_xpt, meta = utils.read_XPT(xpt_dir, xpt_dict['ex.xpt'])
    # print(ex_xpt)
    # Do mapping on available columns
    columns_to_map = {"EXROUTE": "Route of Administration Response"}
    dfEX_map = utils.column_mapping(
		columns_to_map, 
		ex_xpt, 
		json_file)
    return dfEX_map, meta


def TS_dataframe(xpt_dir, xpt_dict, json_file):
    # TODO: Rewrite to add rows, not columns
    TS_xpt, meta = utils.read_XPT(xpt_dir, xpt_dict['ts.xpt'])

    # Replace original TS data with non-mapping rows and raw mapping rows
    dfTS_orig = TS_xpt[-TS_xpt["TSPARMCD"].isin(["ROUTE", "SPECIES", "STRAIN"])] #non mapping rows
    dfTS = TS_xpt[TS_xpt["TSPARMCD"].isin(["ROUTE", "SPECIES", "STRAIN"])] #rows to map
    dfTS_raw = dfTS.copy() #duplicate of mapping rows
    dfTS_raw["TSPARMCD"] = dfTS_raw["TSPARMCD"].astype(str) + "_raw" #original raw data for mapping rows
    TS_xpt = pandas.concat([dfTS_orig, dfTS_raw], ignore_index=True) #raw and non-mapping rows combined

    #Map the TS rows of interest
    columns_to_map = {"ROUTE": "Route of Administration Response",
                      "SPECIES": "Species",
                      "STRAIN": "Strain/Substrain"}

    for index, row in dfTS.iterrows():
        for col_short in row[["TSPARMCD"]]:
            col_long = columns_to_map.get(col_short)

            row[["TSVAL"]] = row[["TSVAL"]].map(
                lambda x: do_mapping(col_long, x, json_file)
            )

    dfTS_map = pandas.concat([TS_xpt, dfTS], ignore_index=True)

    return dfTS_map, meta


def DS_dataframe(xpt_dir, xpt_dict, json_file):

    ds_xpt, meta = utils.read_XPT(xpt_dir, xpt_dict['ds.xpt'])

    # Do mapping on available columns
    columns_to_map = {"DSDECOD": "Standardized Disposition Term"}
    dfDS_map = utils.column_mapping(
        columns_to_map, ds_xpt, json_file
    )

    return dfDS_map, meta


def DM_dataframe(xpt_dir, xpt_dict, json_file):

    dm_xpt, meta = utils.read_XPT(xpt_dir, xpt_dict['dm.xpt'])

    columns_to_map = {"SEX": "Sex"}
    dfDM_map = utils.column_mapping(
        columns_to_map, dm_xpt, json_file
    )

    return dfDM_map, meta
