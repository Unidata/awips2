
import pypies
from timeit import Timer
ntrials = 10

def readObject():
    f = open('/tmp/javaOut')
    data = f.read()
    f.close()
    return data

def deserialize(data):
    dsm = pypies.DynamicSerializationManager.DynamicSerializationManager()
    obj = dsm.deserializeBytes(data)
    return obj
    
def main():
    timer = Timer("deserialize(data)", "from __main__ import deserialize, readObject; data = readObject()")
    print "deserializing took", sum(timer.repeat(ntrials,1))/ntrials,'seconds'
    obj = readObject()
    b = deserialize(obj)
    printout(b)
    
def printout(obj):    
    print "result", obj
    print "groupName", obj.getGroupName()
    print "size", obj.getSize()
    print "otherName", obj.getOtherName()
    print "nullTest", obj.getNullTest()
    print "getValue", obj.getValue()
    print "good", obj.getGood()
    print "floatValue", obj.getFloatValue()    
    print "floatArray", obj.getFloatArray()
    print "intArray", obj.getIntArray()
    print "testMap", obj.getTestMap()
    print "testSet", obj.getTestSet()
    print "someEnum", obj.getSomeEnum()



if __name__ == '__main__':
    main()