
import pypies, numpy
from dynamicserialize.dstypes.com.raytheon.uf.common.pypies import TestStoreRequest
from timeit import Timer
ntrials = 10

def main():    
    timer = Timer("serialize(obj)", "from __main__ import serialize, makeObject; obj = makeObject()")
    print "serializing took", sum(timer.repeat(ntrials,1))/ntrials,'seconds'
    obj = makeObject()
    b = serialize(obj)
    writeOut(b)

def makeObject():    
    obj = TestStoreRequest()
    obj.setGroupName("/155/uyp/gfe")
    obj.setSize(8)
    obj.setOtherName("2382;332")
    obj.setNullTest(None)
    obj.setValue(4.5)
    obj.setGood(False)
    obj.setFloatValue(3.3)
    fa = numpy.zeros((302088,), numpy.float32)
    fa[0] = 5.5
    obj.setFloatArray(fa)
    ia = numpy.zeros((302088,), numpy.int32)
    ia[0] = 18
    obj.setIntArray(ia)
    obj.setTestMap({'tue':4.3, 'wed':5.6})
    obj.setTestSet(set([4, 2, 8]))
    obj.setSomeEnum('THIRD')
    return obj

def serialize(obj):
    dsm = pypies.DynamicSerializationManager.DynamicSerializationManager()
    b = dsm.serializeObject(obj)
    return b

def writeOut(b):
    f = open('/tmp/pyOut', 'w')        
    f.write(b)
    f.flush()
    f.close()

if __name__ == '__main__':
    main()
    