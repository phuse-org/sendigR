# Supress warnings ....
import warnings

import pandas

warnings.filterwarnings("ignore")

from io import StringIO

import json
import shutil
import pyreadstat
import csv
from xptcleaner import CONFIG

from .cdisc_mapping.logger import logger
from .cdisc_mapping.file_mapping import xpt_mapping
from .cdisc_mapping.vocab_management import create_vocab

# Set up log file
logfile = CONFIG["log_file"]
print(logfile)
LOG = logger.setup_applevel_logger(file_name=str(logfile))


def gen_vocab(in_file, out_path):
    """
    Writes json files for vocab mappings.  Keys are synonyms and
    values are the preferred terms.  Vocabs are defined by column values
    from the tab-separated file.

    """
    section_cols = CONFIG["section_cols"]

    vocab = create_vocab.gen_vocab(in_file, section_cols)

    with open(out_path, "w") as out_file:
        out_file.write(json.dumps(vocab, indent=4, sort_keys=True))


def compare_vocabs(vocab_path_a, vocab_path_b):
    """
    Reports if vocabs are same, and different terms in each
    """
    with open(vocab_path_a, "r") as json_file:
        vocab_a = json.loads(json_file.read(), strict=False)

    with open(vocab_path_b, "r") as json_file:
        vocab_b = json.load(json_file)

    a_not_in_b, b_not_in_a = create_vocab.compare_vocabs(vocab_a, vocab_b)

    if len(a_not_in_b) != 0 and len(b_not_in_a) != 0:
        LOG.warning(
            "Vocabs not matching:  vocab a not in b: "
            + str(len(a_not_in_b))
            + " vocab b not in a "
            + str(len(b_not_in_a))
        )

def standardize_file(input_xpt_dir, output_xpt_dir, json_file):
    """
    Standardizes provided .xpt file
    """
# Read in the file
    is_valid, dm_dict, optional_files_dict, xpt_extra = xpt_mapping.check_valid_files(input_xpt_dir)
    if is_valid:
        dfDM, meta = xpt_mapping.DM_dataframe(input_xpt_dir, dm_dict, json_file)
        pyreadstat.write_xport(dfDM, output_xpt_dir + "/" + "dm.xpt",
                               file_format_version=5,
                               table_name="DM",
                               file_label="Demographics",
                               column_labels=meta.column_labels
                               )

        if "ex.xpt" in optional_files_dict.keys():
            dfEX, meta = xpt_mapping.EX_dataframe(input_xpt_dir, optional_files_dict, json_file)
            pyreadstat.write_xport(dfEX, output_xpt_dir + "/" + "ex.xpt",
                                   file_format_version=5,
                                   table_name="EX",
                                   file_label="Exposure",
                                   column_labels=meta.column_labels
                                   )

        if "ts.xpt" in optional_files_dict.keys():
            dfTS, meta = xpt_mapping.TS_dataframe(input_xpt_dir, optional_files_dict, json_file)
            pyreadstat.write_xport(dfTS, output_xpt_dir + "/" + "ts.xpt",
                                   file_format_version=5,
                                   table_name="TS",
                                   file_label="Trial Summary",
                                   column_labels=meta.column_labels
                                   )

        if "ds.xpt" in optional_files_dict.keys():
            dfDS, meta = xpt_mapping.DS_dataframe(input_xpt_dir, optional_files_dict, json_file)
            pyreadstat.write_xport(dfDS, output_xpt_dir + "/" + "ds.xpt",
                                   file_format_version=5,
                                   table_name="DS",
                                   file_label="Disposition",
                                   column_labels= meta.column_labels
                                   )

        if "mi.xpt" in optional_files_dict.keys():
            dfMI, meta = xpt_mapping.MI_dataframe(input_xpt_dir, optional_files_dict, json_file)
            pyreadstat.write_xport(dfMI, output_xpt_dir + "/" + "mi.xpt",
                                   file_format_version=5,
                                   table_name="MI",
                                   file_label="Microscopic Findings",
                                   column_labels=meta.column_labels
                                   )
        for xpt in xpt_extra:
            shutil.copy(input_xpt_dir + "/" + xpt, output_xpt_dir + "/" + xpt)

    else:
        LOG.error("Missing required xpt files")
