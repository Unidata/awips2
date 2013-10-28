/*
 * Created on Jul 11, 2003
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package ohd.hseb.util;

/**
 * @author gobsc
 *
 * To change the template for this generated type comment go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
public class NameValuePair
{
    private String _name;
    private String _value;
    
    public NameValuePair()
    {
    	
    }
  
    public NameValuePair(String name, String value)
    {
        setName(name);
        setValue(value);	
    }

	public void setName(String name)
	{
		this._name = name;
	}

	public String getName()
	{
		return _name;
	}

	public void setValue(String value)
	{
		this._value = value;
	}

	public String getValue()
	{
		return _value;
	}
    
}
