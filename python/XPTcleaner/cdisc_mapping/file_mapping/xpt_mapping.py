import pandas
import glob
import json

from cdisc_mapping import utils
from cdisc_mapping import logger

pandas.options.mode.chained_assignment = None

LOG = logger.get_logger(__name__)

def check_valid_files(input_dir):
    """
    Returns true if mi.xpt is available, otherwise false
    """
    input_xpt = glob.glob(input_dir + '/*.xpt')
    input_xpt_cleaned = [x.lower().split("\\")[-1] for x in input_xpt]
    required_xpts = ["dm.xpt"]
    optional_xpts = ["mi.xpt", "ds.xpt", "ex.xpt", "ts.xpt"]

    # Get the XPT files that will not be mapped
    combo_xpts = [required_xpts, optional_xpts]
    xpt_extra = [v for v in input_xpt_cleaned if v not in set().union(*combo_xpts)]

    # Check if dm.xpt exists
    dm_found = len(set(required_xpts) & set(input_xpt_cleaned))
    # Get names of optional xpt domains
    optional_files_found = set(optional_xpts) & set(input_xpt_cleaned)

    # If number of required files equals the number of required files
    # found on the filesystem, continue on
    if dm_found == len(required_xpts):
        return True, optional_files_found, xpt_extra
    else:
        return False, optional_files_found, xpt_extra

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
        return contents[column][stan_term]
    elif stan_term == '':
        # Possibly warn this is blank?
        return "NA"
    else:
         LOG.warning(
           "Term: " + term + " and standardized term: " + stan_term + " in col: " + column + " did not map"
         )
        #return "UNMAPPED"
        # leave the raw value as it is, it is up to the user to check the log file and correct all the mapping
         return term

def MI_dataframe(xpt_dir, json_file):

    mi_xpt, meta  = utils.read_XPT(xpt_dir, "/mi.xpt")

    # General Histopathologic Exam, Qual
    dfMI= mi_xpt[mi_xpt["MITESTCD"] == "GHISTXQL"]
    if (len(dfMI) == 0):
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
        "MISPEC": "Specimen",
        # Anatomical Location is not for MIANTREG, it is for FXLOC
        # "MIANTREG": "Anatomical Location",
        "MISEV": "SEND Severity"
    }

    dfMI_map = utils.column_mapping(columns_to_map, dfMI, json_file)

    return dfMI_map, meta


def EX_dataframe(xpt_dir, json_file):

    # EX.xpt takes priority for route over TS.xpt
    # TODO: need flexible name strategy
    ex_xpt, meta = utils.read_XPT(xpt_dir, "/ex.xpt")
    # print(ex_xpt)
    # Do mapping on available columns
    columns_to_map = {"EXROUTE": "Route of Administration Response"}
    dfEX_map = utils.column_mapping(columns_to_map, ex_xpt, json_file)
    return dfEX_map, meta


def TS_dataframe(xpt_dir, json_file): 
    # TODO: Rewrite to add rows, not columns
    TS_xpt, meta = utils.read_XPT(xpt_dir, "/ts.xpt")

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


def DS_dataframe(xpt_dir, json_file):

    ds_xpt, meta = utils.read_XPT(xpt_dir, "/ds.xpt")

    # Do mapping on available columns
    columns_to_map = {"DSDECOD": "Standardized Disposition Term"}
    dfDS_map = utils.column_mapping(
        columns_to_map, ds_xpt, json_file
    )

    return dfDS_map, meta


def DM_dataframe(xpt_dir, json_file):

    dm_xpt, meta = utils.read_XPT(xpt_dir, "/dm.xpt")

    columns_to_map = {"SEX": "Sex"}
    dfDM_map = utils.column_mapping(
        columns_to_map, dm_xpt, json_file
    )

    return dfDM_map, meta
