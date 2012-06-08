package gov.noaa.nws.ncep.viz.rsc.plotdata.conditionalfilter;

import java.util.List;

/**
 * Defines a ConditionalFilter interface.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 04/2012      #615        S. Gurung   Initial Creation.
 * 
 * </pre>
 * 
 * @author sgurung
 * @version 1
 */
public interface IConditionalFilter {

    /**
     * Get the size of the conditional filter
     * 
     * @return the number of discrete conditions in the map
     */
    public abstract int getSize();

    /**
     * Get the plot parameter name
     * 
     * @return
     */
    public abstract String[] getParamNames();

    /**
     * Get the constraint
     * 
     * @return
     */
    public abstract String[] getConstraintTypes();

    /**
     * Get the value
     * 
     * @return
     */
    public abstract String[] getValues();

    /**
     * Get the list of conditions
     * 
     * @return
     */
    public abstract List<ConditionalFilterElement> getConditionalFilterElements();

    /**
     * Get the name of the 
     * 
     * @return  name
     */
    public abstract String getName();    
  
    /**
     * The  was renamed
     * 
     * @param name
     */
    public void setName(String name);

    /**
     * Clone the 
     * 
     * @return
     */
    public abstract IConditionalFilter clone();
}
