from app import LOG


def standardize(df):
    """
    Returns standardized text for pandas dataframes.  Changes text to all uppercase, multiple
    adjustments for reducing whitespaces

    """
    df = df.fillna("")
    df = df.apply(lambda x: x.astype(str).str.upper())
    df = df.replace(
        to_replace=[",", "-", "=", "/", "\[", "]", "\(", "\)", ";", ":", "\.", "\\\\"],
        value=" ",
        regex=True,
    ).astype(
        str
    )  # multiple white-space to 1 space "\\"
    df = df.apply(lambda x: x.str.strip() if x.dtype == "object" else x)
    df = df.replace(to_replace=" +", value=" ", regex=True).astype(
        str
    )  # multiple white-space to 1 space "\\"

    return df


def standardize_text(s):
    """
    Returns standardized text
    """
    import re
    try:
        standard_s = re.sub('[^0-9a-zA-Z]+', ' ', str(s)).upper()
        return standard_s.strip()
    except Exception as e:
        LOG.error("ERROR exception caught:  " + str(e))
    return False

