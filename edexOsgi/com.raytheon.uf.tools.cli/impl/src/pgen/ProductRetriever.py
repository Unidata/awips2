import os
from ufpy import ThriftClient
from dynamicserialize.dstypes.com.raytheon.uf.common.datastorage.records import StringDataRecord
from dynamicserialize.dstypes.com.raytheon.uf.common.datastorage.records import ByteDataRecord
from dynamicserialize.dstypes.gov.noaa.nws.ncep.common.dataplugin.pgen.request import RetrieveAllProductsRequest


class ProductRetriever:
    """ Retrieves all PGEN products for a given Activity (dataURI) and writes them to separate files."""

    def __init__(self,dataURI,label):
         self.dataURI = dataURI
         self.label = label
         self.fullpath = False
         self.outdir = os.getcwd()
         self.host = os.getenv("DEFAULT_HOST", "localhost")
         self.port = os.getenv("DEFAULT_PORT", "9581")
         self.client = ThriftClient.ThriftClient(self.host, self.port)

    def setOutputDir(self, outdir):
         self.outdir = outdir
    
    def setFullpath(self, fullpath):
         self.fullpath = fullpath

    def _writeout(self,filename,bytes):
        outname = self.outdir + str(os.sep) + filename
        f = open(outname,"wb")
        f.write(bytes)
        f.close()

    def getProducts(self):
         """ Sends ThriftClient request and writes out received files."""
         req = RetrieveAllProductsRequest()
         req.setDataURI(self.dataURI)
         resp = self.client.sendRequest(req)
         #os.mkdir(self.outdir)
         for item in resp:
             if item.getName() == "ActivityXML":
                 if self.label.endswith(".xml"):
                     filename = self.label
                 else:
                     filename = self.label + ".xml"
             else:
                 filename = item.getName()
             
             if ( self.fullpath ):
                 path = self.dataURI
                 fname = path.replace("/", ".") + "$" + filename
                 filename = fname.lstrip().strip(".").replace("..", ".")
         
         
             if isinstance(item, StringDataRecord):
                 self._writeout(filename,item.getStringData()[0])
             elif isinstance(item, ByteDataRecord):
                 self._writeout(filename,item.getByteData())
             print "Extracted... " + filename

         return resp
             