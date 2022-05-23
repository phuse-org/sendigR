import pandas
import os
import json

from xptcleaner.cdisc_mapping.logger import logger
from xptcleaner.cdisc_mapping.utils import utils
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

    xpt_noMap_dict = dict((d, files_dict.pop(d, None)) for d in xpt_noMap)
    required_dict = dict((d, files_dict.pop(d, None)) for d in required_xpts)

    # Logic depending if dm.xpt found or not
    if None not in required_dict.values(): #dm found
        return True, required_dict, files_dict, xpt_noMap_dict
    else: #dm not found
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
        print("Column: " + column + "was not found in keys")
    else:
        # consider doing text standardization
        stan_term = utils.standardize_text(term)
        if stan_term in contents[column]:
            mapped_term = contents[column][stan_term]
            if (stan_term != mapped_term):
                LOG.info(
                    '"' + term + '","' + stan_term + '","' + mapped_term+ '", ' + column
                )
            return contents[column][stan_term]
        elif stan_term == '':
            # Possibly warn this is blank?
            return ""
        else:
            LOG.warning(
                '"' + term + '","' + stan_term + '",, ' + column
            )
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
    TS_xpt, meta = utils.read_XPT(xpt_dir, xpt_dict['ts.xpt'])
    #Map the TS rows of interest
    columns_to_map = {"ROUTE": "Route of Administration Response",
                      "SPECIES": "Species",
                      "STRAIN": "Strain/Substrain"}

    for index, row in TS_xpt.iterrows():
        for col_short in row[["TSPARMCD"]]:
            col_long = columns_to_map.get(col_short)
            if (col_long is not None):
                TS_xpt.at[index, "TSVAL"]=do_mapping(col_long, TS_xpt.at[index, "TSVAL"], json_file)
    return TS_xpt, meta


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
