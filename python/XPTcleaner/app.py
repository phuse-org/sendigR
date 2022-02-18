import click
import json
import yaml
import json

from cdisc_mapping import logger
from cdisc_mapping import utils
from cdisc_mapping import vocab_management

LOG = logger.setup_applevel_logger(file_name="app_debug.log")

# TODO: Make sure this file exists, error if doesn't
# labels: good first issue
with open('config.yaml') as f:
    CONFIG = yaml.load(f, Loader=yaml.FullLoader)

@click.group()
def main():
    pass


@main.command()
@click.argument("in_file")
@click.argument("out_path")
def gen_vocab(in_file, out_path):
    """
    Writes json files for vocab mappings.  Keys are synonyms and
    values are the preferred terms.  Vocabs are defined by column values
    from the tab-separated file.

    """
    section_cols = CONFIG['section_cols']

    vocab = vocab_management.gen_vocab(in_file, section_cols)
    with open(out_path, "w") as out_file:
        out_file.write(json.dumps(vocab, indent=4, sort_keys=True))


@main.command()
@click.argument("vocab_path_a")
@click.argument("vocab_path_b")
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

'''
@main.command()
@click.argument('column')
@click.argument('term')
def example_mapping(column, term, extra_logic=False):
    json_file_path = "generated_vocabs/current.json"

    with open(json_file_path, 'r') as j:
        contents = json.loads(j.read())

    # make sure we have the column
    assert column in contents.keys()
    
    # consider doing text standardization?
    stan_term = utils.standardize_text(term)
    if stan_term in contents[column]:
        print(contents[column][stan_term])
    else:
        # example of doing more mapping logic
        # example_mapping(tissue, my_term, extra_logic=True)
        if extra_logic:
            # do_something_more()
            pass
        else:
            LOG.warning("Term not found:  " + str(term) + ":" + str(stan_term))
'''

if __name__ == "__main__":
    main()
