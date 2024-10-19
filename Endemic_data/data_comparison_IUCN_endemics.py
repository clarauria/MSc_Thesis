from typing import List, Optional
import re
import pandas as pd
import numpy as np
from dataclasses import dataclass



conditions=["H", "P", "G","T","C", "F", "L"]

conditions2={
    'Data Deficient': "DD",
    "Least Concern" : "LC",
    "Near Threatened" : "NT",
    "Vulnerable" : "VU",
    "Endangered": "EN",
    "Critically Endangered": "CR",
    "Extinct in the Wild": "EW",
    "Extinct": "EX",
    "NA":"NA"
}


@dataclass
class Specie:
    """ store redlist species"""
    scientic_name:str
    state: str
    synonims: List['Synonim'] = None





@dataclass
class Synonim:

    scientific_name:str
    full_name:str
    author:str
    sub_name:str
    def __post_init__(self):
        self.name=self.adapt_name()

    def adapt_name(self):
        try:
            return self.full_name.replace(self.author,"").strip()
        except:
            return







class Data:
    """ Add 0-1 to existing categories"""
    def __init__(self,
                 path:str):
        self._path=path
        self.data=self._load_data()
        self._iter_data()

    def _load_data(self):
        return pd.read_excel(self._path)



    @staticmethod
    def _create_new_columns(data):
        column_names=["H", "P", "G","T","C", "F", "L"]
        for column in column_names:
            data[column]=None
        pass
        return data

    def _iter_data(self):
        data=self._create_new_columns(self.data)

        for i,row in data.iterrows():
            letter=row['Island occurence']
            if letter is np.nan:
                continue
            for word in conditions:
                if word in letter:
                    data.at[i,word]=1
                else:
                    data.at[i,word]=0
                continue

class Insects:
    def __init__(self):

        self._load_data()
        self.get_species()
        self.iter_data()

    def _load_data(self):
        # Pass different data to process: madeira, azores, cavo verde...!!
        self.insects = pd.read_csv(
            r"\assessments.csv",
            delimiter=',')
        self.data=pd.read_excel(
            r"Endemic species madeira.xlsx")
        self.synonims = pd.read_csv(
            r"\synonyms.csv",
            delimiter=',')

    @staticmethod
    def search_subspecies(name:str):
        """ Check if its a subpecie"""
        name=str(name)
        match = re.search(r'ssp\.\s+(\w+)', name)

        if match:
            return match.group(1).lower()
        match = re.search(r'subsp\.\s+(\w+)', name)
        if match:
            return match.group(1).lower()
        return "nan"


    def get_synonims(self)->List['Synonim']:
        """iter trhrough the synonim file and store synonims in a dictionary"""

        synonims=[]
        pass
        for i,row in self.synonims.iterrows():
            syn=Synonim(
                scientific_name=row['scientificName'],
                author=row['speciesAuthor'],
                full_name=row['name'],
                sub_name=self.search_subspecies(row['name'])
            )
            synonims.append(syn)
        pass

       #result_dict = self.synonims.groupby('scientificName')['name'].apply(lambda x: list(x.unique())).to_dict()
        return synonims



    def get_species(self)->List['Specie']:
        """ Iter through the redlist and store basic info and connect with links"""
        self.synonims_info=self.get_synonims()
        pass
        species=[]

        for _,row in self.insects.iterrows():
            pass
            spec=Specie(
                scientic_name=row['scientificName'],
                state=row['redlistCategory'],
                synonims=[x for x in self.synonims_info
                          if x.scientific_name == row['scientificName']]
            )
            species.append(spec)
        self.species=species
        pass
        pass


    def iter_data(self):
        """ Iter """

        check_2=[]
        pass
        for i,row in self.data.iterrows():
            specie = row['Species'].split('(')[0].strip()
            if 'ssp.' in str(row['Species']):
                check = self.check_subspecies(name=row['Species'])
                pass
            else:
                 # TODO: check first if there is a subspecie
                check=self.check_species(name=specie)
            check_2.append(check)
            continue
        self.data['check']=check_2
        return self.pul_dataframe()



    def check_species(self, name:str):

        check=0
        state=None
        name_2 = name.split('(')[0].strip()
        for item in self.species:
            pass
            if name_2 == item.scientic_name or any(name_2 == b.name for b in item.synonims):
                check=+1
                state=item.state
            else:
                continue
        if check == 0:
            return 'NA'
        else:
            return state


    def check_subspecies(self,name):
        check = 0
        state = None
        pass
        name_2 = name.split('(')[0].strip()
        subs_name=self.search_subspecies(name)
        pass
        for item in self.species:
            if (name_2 == item.scientic_name
                    or any(name_2 == b.name for b in item.synonims)
                    or any(subs_name in b.sub_name for b in item.synonims)):
                check = +1
                state = item.state
            else:
                continue
        if check == 0:
            return 'NA'
        else:
            return state
        pass


    def pul_dataframe(self):
        """ repara el dataframe para el final"""
        self.data['check']=self.data['check'].replace(conditions2)
        column_names = ["H", "P", "G", "T", "C", "F", "L"]
        for column in column_names:
            self.data[column] = None
        """ Fix the genus"""

        self.data['Endemic genus'].fillna(value='0',inplace=True)


        columns=['Species','Order','Class', 'Phylum', 'Kingdom', 'Endemic genus', 'check', 'Redlist']
        self.data=self.data[columns]
        return self.data


# TO run:
# generate data with the following script
    #data_=Data('previous_data/data.xlsx')
    #a.data.to_excel(r'results.xlsx',index=False) # save it

# generate the final data
a=Insects().iter_data()
