

import random
import StatsThread

pluginNames = ['grib', 'obs', 'modelsounding', 'acarssounding', 'satellite', 'obs']
reqNames = [ 
             "StoreRequest",    
             "RetrieveRequest",
             "DatasetNamesRequest",
             "DatasetDataRequest",
             "GroupsRequest",
             "DeleteRequest",
             "DeleteFilesRequest",
             "CreateDatasetRequest"
             ]

def main():
    st = StatsThread.StatsThread()    
    for plugin in pluginNames:
        fn = '/awips2/edex/data/hdf5/' + plugin + '/blahblahblah.h5'
        count = random.randint(5, 20000)
        for x in range(count):
            reqIndex = random.randint(0, 7)
            d = {'file': fn, 'time':random.uniform(0, 5), 'request':reqNames[reqIndex]}
            st.addRecord(d)
    
    print st._createLogStatement(st.minuteStats)
            


if __name__ == '__main__':
    main()