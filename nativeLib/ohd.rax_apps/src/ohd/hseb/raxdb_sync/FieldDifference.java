package ohd.hseb.raxdb_sync;

public class FieldDifference
{
        private String _name = null;
        private String _ihfsValue = null;
        private String _raxValue = null;
        
        public FieldDifference ()
        {

        } // end of default FieldDifference constructor
        
        public FieldDifference (String name, String ihfsValue, String raxValue)
        {
            setName(name);
            setIhfsValue(ihfsValue);
            setRaxValue(raxValue);
        } // end of FieldDifference constructor
        
        private void setName(String name)
        {
            _name = name;
        }
        
        public String getName()
        {
            return _name;
        }
        
        private void setIhfsValue(String ihfsValue)
        {
            _ihfsValue = ihfsValue;
        }
        
        public String getIhfsValue()
        {
            return _ihfsValue;
        }

        private void setRaxValue(String raxValue)
        {
            _raxValue = raxValue;
        }

        public String getRaxValue()
        {
            return _raxValue;
        }
            
} // end of FieldDifference class
