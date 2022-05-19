import logging
import sys

APP_LOGGER_NAME = "CDISC Mapper"


def setup_applevel_logger(logger_name=APP_LOGGER_NAME, is_debug=True, file_name=None):
    logger = logging.getLogger(logger_name)
    logger.setLevel(logging.DEBUG if is_debug else logging.INFO)

    formatter = logging.Formatter("%(asctime)s,%(levelname)s,%(message)s", "%Y-%m-%d %H:%M:%S")

    sh = logging.StreamHandler(sys.stdout)
    sh.setFormatter(formatter)
    logger.handlers.clear()
    logger.addHandler(sh)

    if file_name:
        fh = logging.FileHandler(file_name)
        fh.setFormatter(formatter)
        logger.addHandler(fh)
        # Add the first line as the header
        logger.info("Original Term,Standardized Term,Mapped Term,CodeList")

    return logger


def get_logger(module_name):
    return logging.getLogger(APP_LOGGER_NAME).getChild(module_name)
