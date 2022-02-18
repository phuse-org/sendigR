import pandas

from cdisc_mapping import logger
from cdisc_mapping import utils

LOG = logger.get_logger(__name__)

def gen_vocab(in_file, section_list):
    """
    Returns dict of dicts representing each section list 
    synonym -> preferred term mapping from tab-separated 
    term file.
    """

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
        vocab_dict[row['CDISC Submission Value']] = row['CDISC Submission Value']
        
        return vocab_dict

    result_dict = {}
    vocab_df = pandas.read_csv(open(in_file, 'r'), sep="\t")
    for section in section_list:
        vocab_dict = {}

        # Filter dataframe to necessary section

        # TODO: Check for Codelist Name and Synonym column
        # labels: good first issue
        sect_df = vocab_df[vocab_df["Codelist Name"].isin([section])].drop_duplicates()

        # Go through the rows and add the data into the blank 
        # vocab dict, replace missing values with empty string
        sect_df.apply(lambda row: make_dict(row.fillna(""), vocab_dict), axis=1)
        result_dict[section] = vocab_dict

    return result_dict

def compare_vocabs(vocab_a, vocab_b):
    """
    Returns a dict of keys in vocab_a that aren't in b, 
    and keys in b not in a
    """
    vocab_a_terms = []
    vocab_b_terms = []

    groups_a = vocab_a.keys()
    groups_b = vocab_b.keys()

    diff_group_avb = list(set(groups_a)-list(set(groups_b)))
    diff_group_bva = list(set(groups_b)-list(set(groups_a)))

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
