from cdisc_mapping import logger
from cdisc_mapping import file_mapping
import chardet
import pandas

LOG = logger.get_logger(__name__)


def standardize_text(s):
    """
    Returns standardized text
    """
    import re

    try:
        standard_s = re.sub("[^0-9a-zA-Z]+", " ", str(s)).upper()
        return standard_s.strip()
    except Exception as e:
        LOG.error("ERROR exception caught:  " + str(e))
    return False


def column_mapping(columns_to_map, df, json_file):
    for column in columns_to_map.keys():
        df[column + "_raw"] = df[column]
        df[column] = df[column].map(
            lambda x: file_mapping.do_mapping(columns_to_map[column], x, json_file)
        )

    return df


def read_XPT(file_path, file_name):
    with open(file_path + file_name, "rb") as f:
        result = chardet.detect(f.read())
    xpt_df = pandas.read_sas(file_path + file_name, encoding=result["encoding"])

    return xpt_df


def merge_fix_cols(df_left, df_right, uniqueID):
    df_merged = pandas.merge(
        df_left, df_right, how="left", left_on=uniqueID, right_on=uniqueID
    )
    for col in df_merged:
        if col.endswith("_x"):
            df_merged.rename(columns=lambda col: col.rstrip("_x"), inplace=True)
        elif col.endswith("_y"):
            to_drop = [col for col in df_merged if col.endswith("_y")]
            df_merged.drop(to_drop, axis=1, inplace=True)
        else:
            LOG.error("No column conflicts during merge")

    return df_merged
