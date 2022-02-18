# CDISC Mapping

This is a repository containing the code to generate the vocabularies for mapping SEND + custom terms to datasets.  

## Execution

```
➜ python app.py --help                                              
Usage: app.py [OPTIONS] COMMAND [ARGS]...

Options:
  --help  Show this message and exit.

Commands:
  gen-vocab  Writes json files for vocab mappings.
```
### Example
```
python app.py gen-vocab ../input/SEND\ Terminology.txt output.json
```

## CDISC Mapping notes

### Planning/Thinking
1. Decouple vocabulary creation/management from mapping (another module)
    - Take CDISC Vocab, make a preferred term synonym mapping.  This can then be edited for additonal needed synonyms after the first execution.
   ``` json
   {
    "vocab": {
        "": "U",
        "COMBINED": "U",
        "F": "F",
        "FEMALE": "F",
        "M": "M",
        "MALE": "M",
        "SEX": "SEX",
        "U": "U",
        "UNDIFFERENTIATED": "UNDIFFERENTIATED",
        "UNKNOWN": "U"
    }
- Update vocabulary : check for new synonyms, check for mapping changes, check for new terms.  Make sure preferred term is listed as synonym -> for checking.