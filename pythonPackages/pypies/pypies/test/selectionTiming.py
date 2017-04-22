
import h5py, time, numpy
filename='metar-2011-05-03-03.h5'

dsNames = ['temperature', 'windSpeed', 'visibility', 'dewpoint', 'windDir', 'windGust']
#indices = [5, 20, 70, 100, 267, 333, 334, 900, 1201]
indices = []
x = 1
while x < 18000:
   indices.append(x)
   x += 10



def pointRequest():
   try:
      f = h5py.File(filename, 'r')
      results = []
      for dsName in dsNames:
         ds = f[dsName]
         points = numpy.asarray(indices)
         points.resize((len(indices), 1))
         sel = h5py.selections.PointSelection(ds.shape)
         sel.set(points)
         result = ds[sel]
         results.append(result)
      return results
   finally:
      f.close()


def hyperRequest():
   try:
      f = h5py.File(filename, 'r')
      results = []
      for dsName in dsNames:
         ds = f[dsName]
         sel = h5py.selections.HyperSelection(ds.shape)
         sel[()] = False
         for n in indices:
            sel[n] = True
         result = ds[sel]        
         nLines = len(indices)
         if len(result) > nLines:
            result.resize(nLines, len(result) / nLines)
         results.append(result)
      return results
   finally:
      f.close()

def timeIt():
   ntrials = 10
   from timeit import Timer
   timer1 = Timer('pointRequest()', 'from __main__ import pointRequest')
   print "Point request took on average", sum(timer1.repeat(ntrials,1))/ntrials, 'seconds'

   timer2 = Timer('hyperRequest()', 'from __main__ import hyperRequest')
   print "Hyper request took on average", sum(timer2.repeat(ntrials,1))/ntrials, 'seconds'

def main():
   timeIt()


if __name__ == '__main__':
   main()

