package ohd.hseb.pdc_pp;

import java.util.List;

public class PrecipListHolder
{
        private List _instantPrecipTimeSeriesList;
        private List _oneHourPrecipTimeSeriesList;
        private List _threeHourPrecipTimeSeriesList;
        private List _sixHourPrecipTimeSeriesList;
        private List _twentyForHourPrecipTimeSeriesList;
        
        // --------------------------------------------------------------------------------------
        /**
         * @param instantPrecipTimeSeriesList The instantPrecipTimeSeriesList to set.
         */
        public void setInstantPrecipTimeSeriesList(List instantPrecipTimeSeriesList)
        {
            this._instantPrecipTimeSeriesList = instantPrecipTimeSeriesList;
        }
        /**
         * @return Returns the instantPrecipTimeSeriesList.
         */
        public List getInstantPrecipTimeSeriesList()
        {
            return _instantPrecipTimeSeriesList;
        }
        /**
         * @param oneHourPrecipTimeSeriesList The oneHourPrecipTimeSeriesList to set.
         */
        public void setOneHourPrecipTimeSeriesList(List oneHourPrecipTimeSeriesList)
        {
            this._oneHourPrecipTimeSeriesList = oneHourPrecipTimeSeriesList;
        }
        /**
         * @return Returns the oneHourPrecipTimeSeriesList.
         */
        public List getOneHourPrecipTimeSeriesList()
        {
            return _oneHourPrecipTimeSeriesList;
        }
        /**
         * @param threeHourPrecipTimeSeriesList The threeHourPrecipTimeSeriesList to set.
         */
        public void setThreeHourPrecipTimeSeriesList(List threeHourPrecipTimeSeriesList)
        {
            this._threeHourPrecipTimeSeriesList = threeHourPrecipTimeSeriesList;
        }
        /**
         * @return Returns the _3hourPrecipTimeSeriesList.
         */
        public List getThreeHourPrecipTimeSeriesList()
        {
            return _threeHourPrecipTimeSeriesList;
        }
        /**
         * @param sixHourPrecipTimeSeriesList The sixHourPrecipTimeSeriesList to set.
         */
        public void setSixHourPrecipTimeSeriesList(List sixHourPrecipTimeSeriesList)
        {
            this._sixHourPrecipTimeSeriesList = sixHourPrecipTimeSeriesList;
        }
        /**
         * @return Returns the sixHourPrecipTimeSeriesList.
         */
        public List getSixHourPrecipTimeSeriesList()
        {
            return _sixHourPrecipTimeSeriesList;
        }
        /**
         * @param twentyForHourPrecipTimeSeriesList The twentyForHourPrecipTimeSeriesList to set.
         */
        public void setTwentyForHourPrecipTimeSeriesList(List twentyForHourPrecipTimeSeriesList)
        {
            this._twentyForHourPrecipTimeSeriesList = twentyForHourPrecipTimeSeriesList;
        }
        /**
         * @return Returns the twentyForHourPrecipTimeSeriesList.
         */
        public List getTwentyFourHourPrecipTimeSeriesList()
        {
            return _twentyForHourPrecipTimeSeriesList;
        }
        
}
