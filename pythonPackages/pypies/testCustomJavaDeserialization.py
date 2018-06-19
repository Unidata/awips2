
import dynamicserialize
from dynamicserialize.dstypes.java.awt import Point
from dynamicserialize.dstypes.com.raytheon.uf.common.pypies import PointTest

def readObject():
    f = open('/tmp/javaPoints')
    data = f.read()
    f.close()
    return data

def writeObject():
    data = PointTest()
    p1 = Point()
    p1.setX(26)
    p1.setY(9)
    p2 = Point()
    p2.setX(144)
    p2.setY(-7)
    points = [p1, p2]
    data.setPoints(points)
    bytes = dynamicserialize.serialize(data)    
    f = open('/tmp/pythonPoints', 'w')
    f.write(bytes)
    f.close()
    print "wrote to /tmp/pythonPoints"
    
def main():
    data = readObject()
    obj = dynamicserialize.deserialize(data)
    print obj
    print obj.getPoints()
    
    writeObject()


if __name__ == '__main__':
    main()