import pandas
import numpy

from cdisc_mapping import logger
from cdisc_mapping import utils

LOG = logger.get_logger(__name__)


def gen_vocab(in_file, section_list):
    """
    Returns dict of dicts representing each section list
    synonym -> preferred term mapping from tab-separated
    term file.
    """

    result_dict = {}

    for section in section_list:
        # start extensible, finish CDISC to overwrite conflicts
        vocab_dict = {}
        sect_df = pandas.DataFrame()
        for f in in_file:
            # print(f)
            with open(f, 'r') as my_in_file:
                vocab_df = pandas.read_csv(my_in_file, sep="\t")

            # Filter dataframe to necessary section
            temp_df = vocab_df[['CDISC Submission Value', 'CDISC Synonym(s)']][vocab_df["Codelist Name"].isin([section])]
            temp_df['CDISC Submission Value'].str.upper()
            temp_df['CDISC Synonym(s)'].str.upper()
            sect_df = pandas.concat([sect_df, temp_df], ignore_index=True)

        # Go through the rows and add the data into the blank
        # vocab dict, replace missing values with empty string
        sect_df.apply(lambda row: make_dict(row.fillna(""), vocab_dict), axis=1)
        #print("embedded: ", vocab_dict)
        #print(vocab_dict)

        result_dict[section] = vocab_dict

        #Report errors in synonym mismatches b/w 2 files

    #vocab_df.close()

    return result_dict

def make_dict(row, vocab_dict):
    """
        Returns diction of synonyms and preferred terms
        from a pandas row
    """

    synonyms = row['CDISC Synonym(s)'].split(';')

    # Standardize text
    synonyms = list(map(utils.standardize_text, synonyms))
    for synonym in filter(None, synonyms):
        vocab_dict[synonym] = row['CDISC Submission Value']

    # Include the preferred term as another lookup term
    standardized_perferred = utils.standardize_text(row['CDISC Submission Value'])
    vocab_dict[standardized_perferred] = row['CDISC Submission Value']

    return vocab_dict


def compare_vocabs(vocab_a, vocab_b):
    """
    Returns a dict of keys in vocab_a that aren't in b,
    and keys in b not in a
    """
    vocab_a_terms = []
    vocab_b_terms = []

    groups_a = vocab_a.keys()
    groups_b = vocab_b.keys()

    diff_group_avb = list(set(groups_a) - list(set(groups_b)))
    diff_group_bva = list(set(groups_b) - list(set(groups_a)))

    # This should be rare, but if the section columns are different
    # issue a warning -- these are normally specified in the
    # config

    if len(diff_group_avb) > 0 or len(diff_group_bva) > 0:
        LOG.warn("Section headings not matching!")

    # Check the terms themselves
    for group in groups_a:
        vocab_a_terms.extend(vocab_a[group].keys())
    for group in groups_b:
        vocab_b_terms.extend(vocab_b[group].keys())

    a_not_in_b = list(set(vocab_a_terms) - set(vocab_b_terms))
    b_not_in_a = list(set(vocab_b_terms) - set(vocab_a_terms))

    return a_not_in_b, b_not_in_a
