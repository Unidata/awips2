package ohd.hseb.fcstservice;  

import java.util.ArrayList;

public class DataServicesGlobals //This is a singleton class 
{		
		private static int count = 0;
		private static DataServicesGlobals dataServicesGlobals = null;
		private static String missingRepresentation = "-";
		
		public static ArrayList dataServiceObservers = null;
		public static ArrayList dataDetermObservers = null;
		public static ArrayList dataWatSupObservers = null;
		public static ArrayList dataEnsembleObservers = null;
		
		private DataServicesGlobals()
		{
		}

		public static synchronized DataServicesGlobals getSingleInstanceOfDataServicesGlobals()
		{	
			if(dataServicesGlobals == null)
			{
				dataServicesGlobals = new DataServicesGlobals();
			}
			
			dataServiceObservers = new ArrayList();
			dataDetermObservers = new ArrayList();
			dataWatSupObservers = new ArrayList();
			dataEnsembleObservers = new ArrayList();
			
			return dataServicesGlobals;
		}

		public String getMissingRepresentation() {
			return missingRepresentation;
		}

		public void setMissingRepresentation(String missingRepresentation) {
			DataServicesGlobals.missingRepresentation = missingRepresentation;
		}

		public synchronized void incrementCount() 
		{
			count++;
		}

		public synchronized void decrementCount()
		{
			count--;
		}
		public synchronized int getCount()
		{
			return count;
		}
}
