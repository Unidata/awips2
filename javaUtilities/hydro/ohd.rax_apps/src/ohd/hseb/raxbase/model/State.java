package ohd.hseb.raxbase.model;

import ohd.hseb.raxdb.generated.StateRecord;

public class State
{
    private String _state = null;
    private String _countryFips = null;
    private String _name = null;
    private String _ncdc = null;
    private String _statefips = null;

    public State(){}
    
    public State( State state )
    {
        setState( state.getState() );
        setCountryFips( state.getCountryFips() );
        setName( state.getName() );
        setNcdc( state.getNcdc() );
        setStateFips( state.getStateFips() );
    }
    
    public State( StateRecord stateRecord )
    {
        setState( stateRecord.getState() );
        setCountryFips( stateRecord.getCountryfips() );
        setName( stateRecord.getName() );
        setNcdc( stateRecord.getNcdc() );
        setStateFips( stateRecord.getStatefips() );
    }
    
    public static State getState( StateRecord record )
    {
        return new State( record );
    }
    
    public static StateRecord getStateRecord( State state )
    {
        StateRecord record = new StateRecord();
        
        record.setState( state.getState() );
        record.setCountryfips( state.getCountryFips() );
        record.setName( state.getName() );
        record.setNcdc( state.getNcdc() );
        record.setStatefips( state.getStateFips() );
        
        return record;
    }
    
    public String toString()
    {
        return "State = " + _state + 
               " | CountryFips = " + _countryFips + 
               " | Name = " + _name +
               " | Ncdc = " + _ncdc +
               " | StateFips = " + _statefips;
    }
    
    public String keyString()
    {
        return "State = " + _state +
               " | CountryFips = " + _countryFips;
    }
    
    
    public String getState()
    {
        return _state;
    }

    public void setState(String state)
    {
        _state = state ;
    }

    public String getCountryFips()
    {
        return _countryFips;
    }

    public void setCountryFips(String countryfips)
    {
        _countryFips = countryfips ;
    }

    public String getName()
    {
        return _name;
    }

    public void setName(String name)
    {
        _name = name ;
    }

    public String getNcdc()
    {
        return _ncdc;
    }

    public void setNcdc(String ncdc)
    {
        _ncdc = ncdc ;
    }

    public String getStateFips()
    {
        return _statefips;
    }

    public void setStateFips(String statefips)
    {
        _statefips = statefips ;
    }

}
