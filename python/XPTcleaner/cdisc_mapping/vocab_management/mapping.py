from app import LOG

#specimenMapping
#specimen = mapping("Specimen", term??, extra_logic??)
#anatomicalLocation = mapping("Specimen", term??, extra_logic??)

def mapping(column, term, extra_logic=False):
    json_file_path = "generated_vocabs/current.json"

    with open(json_file_path, 'r') as j:
        contents = json.loads(j.read())

    # make sure we have the column
    assert column in contents.keys()

    # consider doing text standardization?
    stan_term = utils.standardize_text(term)
    if stan_term in contents[column]:
        return contents[column][stan_term]
    else:
        # example of doing more mapping logic
        # example_mapping(tissue, my_term, extra_logic=True)
        if extra_logic:
            row['CODE_CONFLICT_FLAG'] = Add_Term('CODE_CONFLICT_FLAG', row, str(column))
            # do_something_more()
            pass
        else:
            LOG.warning("Term not found:  " + str(term) + ":" + str(stan_term))
            row['CODE_CONFLICT_FLAG'] = Add_Term('CODE_CONFLICT_FLAG', row, str(column))


##########################################################################
###                         Main function                               ###
###########################################################################
def main():
    ##In console: wget "https://evs.nci.nih.gov/ftp1/CDISC/SEND/SEND%20Terminology.txt"
    path_to_SENDcodelist = "C:\\Users\c143390\Desktop\SEND Terminology.txt"

    with open(path_to_SENDcodelist, 'rb') as f:
        result = chardet.detect(f.read())  # or readline if the file is large
    dfSENDCodelist = pd.read_csv(path_to_SENDcodelist, encoding=result['encoding'], delimiter="\t").drop_duplicates()

    ##Directory to .xpt file
    mydir = os.path.abspath(r"\\ix1invivo-p\ivdr\InVivoFileFormatAdapter\Mappings\SEND\2021-02-03-11-00-02\done\8413493")

    ##Check which domains have .xpt files in directory
    fMI, fTS, fEX, fDM, fDS = Domain_test(mydir)

    ##Generate MI domain dataframe
    if fMI==None:
        sys.exit("No MI.xpt file detected.") #if no MI domain, then exit with error
    else:
        dfMIRaw = Raw_dataframe(fMI, fTS, fEX, fDM, fDS)

    dfMIRaw, dftemp = dfInitialization(dfMIRaw)
    dfSEXCodelist = dfSENDCodelist[dfSENDCodelist["Codelist Name"].isin(["Sex"])].drop_duplicates()
    dftemp = Gender_Mapping(dftemp, dfSEXCodelist)

    dfSTRAINCodelist = dfSENDCodelist[dfSENDCodelist["Codelist Name"].isin(["Strain/Substrain"])].drop_duplicates()
    dfSPECIESCodelist = dfSENDCodelist[dfSENDCodelist["Codelist Name"].isin(["Species"])].drop_duplicates()
    dftemp = Species_Mapping(dftemp, dfSPECIESCodelist, dfSTRAINCodelist)

    dfSEVERITYCodelist = dfSENDCodelist[dfSENDCodelist["Codelist Name"].isin(["SEND Severity"])].drop_duplicates()
    dftemp = Severity_Mapping(dftemp, dfSEVERITYCodelist)

    dfROUTECodelist = dfSENDCodelist[dfSENDCodelist["Codelist Name"].isin(["Route of Administration Response"])].drop_duplicates()
    dftemp = Route_Mapping(dftemp, dfROUTECodelist)

    dfDISPOSITIONCodelist = dfSENDCodelist[dfSENDCodelist["Codelist Name"].isin(["Standardized Disposition Term"])].drop_duplicates()
    dftemp = Disposition_Mapping(dftemp, dfDISPOSITIONCodelist)
    dftemp = dftemp.fillna("")

    dfSpecimenCodelist = dfSENDCodelist[dfSENDCodelist["Codelist Name"].isin(["Specimen"])].drop_duplicates()
    dfAnatLocCodelist = dfSENDCodelist[dfSENDCodelist["Codelist Name"].isin(["Anatomical Location"])].drop_duplicates()
    dfNonNeoplasmCodelist = dfSENDCodelist[dfSENDCodelist["Codelist Name"].isin(["Non-Neoplastic Finding Type"])].drop_duplicates()

    dftemp.to_csv("//lrlhps/users/c143390/Ontology/Paisley_dftemp_test.csv", index=False, header=True)
    for index in dftemp.index:
        row = Specimen_Mapping(dftemp.iloc[index], dfSpecimenCodelist, dfAnatLocCodelist, dfNonNeoplasmCodelist)
        dftemp.iloc[index] = row
        dftemp.iloc[index] = dftemp.iloc[index].fillna("")
        row = Specimen_Mapping(dftemp.iloc[index], dfSpecimenCodelist, dfAnatLocCodelist, dfNonNeoplasmCodelist)
        val1 = dftemp.at[index, 'MISPEC']
        val2 = dftemp.at[index, 'MIANTREG']
        val3 = dftemp.at[index, 'MISPEC_raw']
        val4 = dftemp.at[index, 'MIANTREG_raw']
        val5 = dftemp.at[index, 'Anatomical_Region_of_Specimen']
        #row = Specimen_Mapping(dftemp.iloc[index], dfSpecimenCodelist, dfAnatLocCodelist, dfNonNeoplasmCodelist)

    # Creating Output File
    #dftemp.to_csv("//lrlhps/users/c143390/Ontology/HistopathTerminology_test_5Aug2021.csv", index=False, header=True)

if __name__ == "__main__":
    start_time = time.time()
    main()
    print("--- %s seconds ---" % (time.time() - start_time))




'''
def example_mapping(column, term, extra_logic=False):
    json_file_path = "generated_vocabs/current.json"

    with open(json_file_path, 'r') as j:
        contents = json.loads(j.read())

    # make sure we have the column
    assert column in contents.keys()

    # consider doing text standardization?
    stan_term = utils.standardize_text(term)
    if stan_term in contents[column]:
        return contents[column][stan_term]
    else:
        # example of doing more mapping logic
        # example_mapping(tissue, my_term, extra_logic=True)
        if extra_logic:
            # do_something_more()
            pass
        else:
            LOG.warning("Term not found:  " + str(term) + ":" + str(stan_term))


def extra_mapping(column, term, extra_logic=True):
    json_file_path = "generated_vocabs/current.json"

    with open(json_file_path, 'r') as j:
        contents = json.loads(j.read())

    # make sure we have the column
    assert column in contents.keys()

    # consider doing text standardization?
    stan_term = utils.standardize_text(term)
    if stan_term in contents[column]:
        return contents[column][stan_term]
    else:
        # example of doing more mapping logic
        # example_mapping(tissue, my_term, extra_logic=True)
        if extra_logic:
            # do_something_more()
            pass
        else:
            LOG.warning("Term not found:  " + str(term) + ":" + str(stan_term))
'''