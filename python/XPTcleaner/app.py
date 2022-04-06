#import click
import json
import yaml
import pandas
import pyreadstat
import shutil

from cdisc_mapping import logger
from cdisc_mapping import file_mapping
from cdisc_mapping import vocab_management
from cdisc_mapping import utils

# Supress pandas warnings ....
# be careful with this
import warnings
warnings.filterwarnings("error")

LOG = logger.setup_applevel_logger(file_name="app_log.csv")

# TODO: Make sure this file exists, error if doesn't
# labels: good first issue
with open("config.yaml") as f:
    CONFIG = yaml.load(f, Loader=yaml.FullLoader)


#@click.group()
def main():
    #standardize_file("//ix1invivo-p/ivdr/InVivoFileFormatAdapter/Mappings/SEND/2020-03-17-11-09-54/done/8381036_12 Feb 2020/","//ix1invivo-p/ivdr/InVivoFileFormatAdapter/Mappings/SEND/2020-03-17-11-09-54/done/8381036_12 Feb 2020/XPT_Cleaner","//lrlhps/lrlhps/users/c143390/Ontology/LRL_cdisc_mapping/cdisc_mapping/generated_vocabs/current.json")

    pass

#@main.command()
#@click.argument("in_file", nargs=-1)
#@click.argument("out_path")
def gen_vocab(in_file, out_path):
    """
    Writes json files for vocab mappings.  Keys are synonyms and
    values are the preferred terms.  Vocabs are defined by column values
    from the tab-separated file.

    """
    section_cols = CONFIG["section_cols"]

    vocab = vocab_management.gen_vocab(list(in_file), section_cols)
    with open(out_path, "w") as out_file:
        out_file.write(json.dumps(vocab, indent=4, sort_keys=True))


#@main.command()
#@click.argument("vocab_path_a")
#@click.argument("vocab_path_b")
def compare_vocabs(vocab_path_a, vocab_path_b):
    """
    Reports if vocabs are same, and different terms in each
    """
    with open(vocab_path_a, "r") as json_file:
        vocab_a = json.loads(json_file.read(), strict=False)

    with open(vocab_path_b, "r") as json_file:
        vocab_b = json.load(json_file)

    a_not_in_b, b_not_in_a = vocab_management.compare_vocabs(vocab_a, vocab_b)

    if len(a_not_in_b) != 0 and len(b_not_in_a) != 0:
        LOG.warning(
            "Vocabs not matching:  vocab a not in b: "
            + str(len(a_not_in_b))
            + " vocab b not in a "
            + str(len(b_not_in_a))
        )


#main.command()
#@click.argument("input_xpt_dir")
#@click.argument("output_xpt_dir")
#@click.argument("json_file")
def standardize_file(input_xpt_dir, output_xpt_dir, json_file):
    """
    Standardizes provided .xpt file
    """

    # Read in the file
    is_valid, dm_dict, optional_files_dict, xpt_extra = file_mapping.check_valid_files(input_xpt_dir)
    #print(optional_files_found)

    if is_valid:
        # TODO: Determine if actual file name is being called
        dfDM, meta = file_mapping.DM_dataframe(input_xpt_dir, dm_dict, json_file)
        # dfDM.to_csv(open(output_xpt_dir + "/sex.txt", 'w'), sep="\t", header=True, index=False)
        pyreadstat.write_xport(dfDM, output_xpt_dir + "/" + "dm.xpt",
                               file_format_version=5,
                               table_name="DM",
                               file_label="Demographics",
                               column_labels=meta.column_labels
                               )


        if "ex.xpt" in optional_files_dict.keys():
            dfEX, meta = file_mapping.EX_dataframe(input_xpt_dir, optional_files_dict, json_file)
            #dfEX.to_csv(open(output_xpt_dir + "/route.txt", 'w'), sep="\t", header=True, index=False)
            pyreadstat.write_xport(dfEX, output_xpt_dir + "/" + "ex.xpt",
                                   file_format_version=5,
                                   table_name="EX",
                                   file_label="Exposure",
                                   column_labels= meta.column_labels
                                   )

        if "ts.xpt" in optional_files_dict.keys():
            dfTS, meta = file_mapping.TS_dataframe(input_xpt_dir, optional_files_dict, json_file)
            #dfTS.to_csv(open(output_xpt_dir + "/species_strain.txt", 'w'), sep="\t", header=True, index=False)
            pyreadstat.write_xport(dfTS, output_xpt_dir + "/" + "ts.xpt",
                                   file_format_version=5,
                                   table_name="TS",
                                   file_label="Trial Summary",
                                   column_labels= meta.column_labels
                                   )

        if "ds.xpt" in optional_files_dict.keys():
            dfDS, meta = file_mapping.DS_dataframe(input_xpt_dir, optional_files_dict, json_file)
            #dfDS.to_csv(open(output_xpt_dir + "/disposition.txt", 'w'), sep="\t", header=True, index=False)
            pyreadstat.write_xport(dfDS, output_xpt_dir + "/" + "ds.xpt",
                                   file_format_version=5,
                                   table_name="DS",
                                   file_label="Disposition",
                                   column_labels= meta.column_labels
                                   )

        if "mi.xpt" in optional_files_dict.keys():
            dfMI, meta = file_mapping.MI_dataframe(input_xpt_dir, optional_files_dict, json_file)
            # dfMI.to_csv(open(output_xpt_dir + "/mi.txt", "w"), sep="\t", header=True, index=False)
            pyreadstat.write_xport(dfMI, output_xpt_dir + "/" + "mi.xpt",
                                   file_format_version=5,
                                   table_name="MI",
                                   file_label="Microscopic Findings",
                                   column_labels=meta.column_labels
                                   )
        for xpt in xpt_extra:
            # just do file copy, no need to re-write
            #df = utils.read_XPT(input_xpt_dir, "/" + xpt)
            #pyreadstat.write_xport(df, output_xpt_dir + "/" + xpt)
            shutil.copy(input_xpt_dir + "/" + xpt, output_xpt_dir + "/" + xpt)


    else:
        LOG.error("Missing required xpt files")


if __name__ == "__main__":
    main()
