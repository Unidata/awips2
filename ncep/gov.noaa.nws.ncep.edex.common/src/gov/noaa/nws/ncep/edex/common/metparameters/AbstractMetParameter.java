package gov.noaa.nws.ncep.edex.common.metparameters;

//import gov.noaa.nws.ncep.edex.common.metparameters.AbstractMetParameter;
//import gov.noaa.nws.ncep.edex.common.metparameters.Amount;

import gov.noaa.nws.ncep.edex.common.metparameters.MetParameterFactory.DeriveMethod;
import gov.noaa.nws.ncep.edex.common.metparameters.MetParameterFactory.NotDerivableException;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.text.NumberFormat;
import java.text.ParseException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Formatter;
import java.util.List;

import javax.measure.converter.ConversionException;
import javax.measure.quantity.Quantity;
import javax.measure.unit.Unit;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.units.UnitAdapter;
import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.DataTime.FLAG;

/**
 * An abstract class for all metParameters. This will hold the value of the
 * parameter and its units.
 * 
 * TODO : add support for the Level/Layer at which the value applies. The level
 * could be a PressureLevel, a Height or 'Surface' or other defined layer. This
 * would allow the derive methods to remove the PressureLevel derive()
 * arguements and do a compatibility check on the other arguements to make sure
 * they are all from the same level. Could also add support for the time or
 * duration for which the value applies.
 * 
 * 
 * TODO : make this a generic for a Quantity? ... AbstractMetParameter<Q extents Quantity>
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 05/05/2011              Greg Hull    Initial creation
 * 06/05/2011              Greg Hull    Added check for infinite recursion in derive method.
 * 10/05/2011              Greg Hull    add dataTime 
 * 10/14/2011              Greg Hull    add setValueFromString
 * 11/14/2011              B. Hebbard   remove standardUnit
 * 06/17/2014              S. Russell   TTR 923: added member, get/set methods associatedMetParam
 * 
 * </pre>
 * 
 * @author ghull
 * @version 1.0
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public abstract class AbstractMetParameter extends Amount implements Quantity, ISerializableObject {

    /**
	 * 
	 */

    @DynamicSerializeElement
    private static final long serialVersionUID = 7369542461296836406L;

    public AbstractMetParameter() {
        super();
        listOfInputMetPrmNamesForDerivingThisMetPrm = new ArrayList<String>(0);
    }

    @DynamicSerializeElement
    private boolean useStringValue; // override to true for String parameters.

    @DynamicSerializeElement
    protected DataTime dataTime;

    //	@DynamicSerializeElement
    //    @XmlJavaTypeAdapter(value = UnitAdapter.class)
    //    private Unit<?> standardUnit;

    //    public Unit<?> getStandardUnit() {
    //        return standardUnit;
    //    }

    // TTR 923, Holds the second MetParameter in a combination 
    // AbstractMetParameter uch as the PTND "button" 
    // (PressPressureChange3HrAndTendency MetParameter )
    @DynamicSerializeElement
    private AbstractMetParameter associatedMetParam = null;

    protected List<String> listOfInputMetPrmNamesForDerivingThisMetPrm;

    public final List<String> getListOfInputMetPrmNamesForDerivingThisMetParameter() {
        return listOfInputMetPrmNamesForDerivingThisMetPrm;
    }

    // TTR 923
    public void setAssociatedMetParam(AbstractMetParameter amp) {
        if (this.associatedMetParam != null)
            this.associatedMetParam = null;

        this.associatedMetParam = amp;
    }

    public AbstractMetParameter getAssociatedMetParam() {
        return this.associatedMetParam;
    }

    /**
     * @return the dataTime
     */
    public final DataTime getDataTime() {
        return dataTime;
    }

    /**
     * @param dataTime
     *            the dataTime to set
     */
    public final void setDataTime(DataTime dataTime) {
        this.dataTime = dataTime;
    }

    /**
     * @return the valueString
     */
    public final String getValueString() {
        return valueString;
    }

    /**
     * @param valueString
     *            the valueString to set
     */
    public final void setValueString(String valueString) {
        this.valueString = valueString;
    }

    /**
     * @param useStringValue
     *            the useStringValue to set
     */
    public final void setUseStringValue(boolean useStringValue) {
        this.useStringValue = useStringValue;
    }

    /**
     * @param standardUnit
     *            the standardUnit to set
     */
    //	public final void setStandardUnit(Unit<?> standardUnit) {
    //		this.standardUnit = standardUnit;
    //	}
    // only one of these may be set at a time. In order to hold a string value
    // the Quantity of the parameter must be Dimensionless
    @DynamicSerializeElement
    protected String valueString; // "" is MISSING, null for non-string values

    // if this list is set then derive() will first look for a derive method using
    // just these parameters.
    @DynamicSerializeElement
    protected ArrayList<String> preferedDeriveParameters;

    /**
     * @return the useStringValue
     */
    public final boolean isUseStringValue() {
        return useStringValue;
    }

    protected AbstractMetParameter(Unit<?> u) {
        super(u);
        valueString = null;
        //standardUnit = u;
    }

    protected AbstractMetParameter(Unit<?> u, DataTime dt) {
        super(u);
        valueString = null;
        //standardUnit = u;
        dataTime = dt;
    }

    protected AbstractMetParameter(String unitStr) { // String ncPrmName ) {

        super(unitStr);
        try {
            valueString = null;
            Unit<?> newUnit = new UnitAdapter().unmarshal(unitStr);
            //standardUnit = newUnit;
        } catch (Exception e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }

    }

    // Override for real Description information
    public String getParameterDescription() {
        return getMetParamName();
    }

    protected void setValueIsString() {
        useStringValue = true;
    }

    //To be removed
    public boolean isUnitCompatible(Unit<?> u) {
        //return standardUnit.isCompatible( u );
        return getUnit().isCompatible(u);
    }

    public boolean isUnitCompatible(String unitName) {
        try {
            //return getStandardUnit().isCompatible( new UnitAdapter().unmarshal(unitName) );
            return getUnit().isCompatible(new UnitAdapter().unmarshal(unitName));
        } catch (Exception e) {
            e.printStackTrace();
        }
        return false;
    }

    public Boolean hasStringValue() {
        return useStringValue;
    }

    public String getStringValue() {
        return (valueString == null ? "" : valueString);

    }

    public Boolean hasValidTime() {
        return (dataTime != null);
    }

    public DataTime getValidTime() {
        return dataTime;
    }

    public Boolean isValidAtTime(DataTime dt) {
        if (dataTime == null) {
            return null;
        }

        if (dataTime.getUtilityFlags().contains(FLAG.PERIOD_USED)) {
            return dataTime.getValidPeriod().contains(dt.getValidTime().getTime());
        } else {
            return dataTime.compareTo(dt) == 0;
        }
    }

    public void setValidTime(DataTime dt) {
        dataTime = dt;
    }

    @Override
    public boolean hasValidValue() {
        if (useStringValue) {
            return (valueString == null ? false : true);
        } else {
            return super.hasValidValue();
        }
    }

    // ?throw exceptions for invalid value or for incompatible units?
    //TODO remove this method
    public Number getValueAs(Unit<?> unitNeeded) {
        if (!hasValidValue()) {
            return null;
        } else if (!isUnitCompatible(unitNeeded)) {
            System.out.println("getValueAs() : asking for incompatible units. " + getUnit().toString() + ", " + unitNeeded.toString());
            return null;
        }

        if (useStringValue) {
            return getMissingValueSentinel();
        } else {
            return super.getValueAs(unitNeeded);
        }
    }

    public Number getValueAs(String unitName) {
        if (!hasValidValue()) {
            return null;
        }
        try {

            if (!isUnitCompatible(unitName)) {
                System.out.println("getValueAs() : asking for incompatible units. " + getUnit().toString() + " , " + unitName);
                return null;
            }

            if (useStringValue) {
                return getMissingValueSentinel();
            } else {
                return super.getValueAs(new UnitAdapter().unmarshal(unitName));
            }

        } catch (Exception e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
        return null;

    }

    // this will work off of the current units so if we need to
    // format at string using different units the value must be 
    // changed before calling this method.
    // the formatStr can be a 'printf' style string or a 
    // parameter-specific tag for abbreviating, triming or 
    // converting the parameter value. In the latter case this 
    // method must be overridden.
    //
    public String getFormattedString(String formatStr) {

        String formattedValueStr = null;

        if (hasStringValue()) {
            formattedValueStr = valueString;
        } else {
            formattedValueStr = getValue().toString();
        }

        if (formatStr == null) {
            return formattedValueStr;
        } else if (formatStr.startsWith("%")) {
            //			double formattedValue = valueAs.doubleValue();
            StringBuilder sb = new StringBuilder();
            Formatter fmtr = new Formatter(sb);
            fmtr.format(formatStr, getValue()); // formattedValue

            formattedValueStr = sb.toString();

            //	sValue = sValue.substring( trim );

            return formattedValueStr;
        } else {
            System.out.println("Sanity Check: Unrecognized format string (" + formatStr + ") for metParameter " + getMetParamName());
            return formattedValueStr;
        }
    }

    // if this 
    public void setStringValue(String sv) throws ConversionException {
        // the units must be dimensionless
        //		if( isUnitCompatible( Unit.ONE ) ) {
        //			throw new ConversionException("Incompatible unit in setStringValue the Quantity "					
        //					+" for this parameter must be Dimensionless." );
        //		}
        setValueToMissing();

        valueString = sv;
    }

    // This is intended for numeric params that may need to parse their values from a string
    //
    public void setValueFromString(String valStr, Unit<?> u) {
        if (hasStringValue()) {
            setStringValue(valStr);
        } else {
            NumberFormat numFmt = NumberFormat.getInstance();
            try {
                setValue(numFmt.parse(valStr), u);
            } catch (ParseException pe) {
                super.setValueToMissing();
            }
        }
    }

    @Override
    public void setValueToMissing() {
        if (useStringValue) {
            valueString = null;
        }

        super.setValueToMissing();
    }

    public Boolean derivable(ArrayList<String> checkedParams, Collection<AbstractMetParameter> inputParams) {

        return (getDeriveMethod(checkedParams, inputParams) != null);
    }

    public Method getDeriveMethod(Collection<AbstractMetParameter> inputParams) {

        ArrayList<String> checkedParams = new ArrayList<String>();

        // if the preferredDeriveParameters list is set then only use these 
        // parameters.
        if (preferedDeriveParameters != null) {

            ArrayList<AbstractMetParameter> inputParamsList = new ArrayList<AbstractMetParameter>();

            for (AbstractMetParameter prm : inputParams) {
                if (preferedDeriveParameters.contains(prm.getMetParamName())) {
                    inputParamsList.add(prm);
                }
            }
            return getDeriveMethod(checkedParams, inputParamsList);
        } else {
            return getDeriveMethod(checkedParams, inputParams);
        }
    }

    // check each of the methods named 'derive' and check to see if the 
    // given inputParams are sufficient to derive this ncParameter.  
    // 
    public Method getDeriveMethod(ArrayList<String> checkedParams, Collection<AbstractMetParameter> inputParams) {

        Method[] deriveMthds = this.getClass().getDeclaredMethods();
        ArrayList<Method> foundDeriveMthds = new ArrayList<Method>();

        // check each derive method to see if its arguments are in the inputParams
        for (Method m : deriveMthds) {
            boolean derivable = true;

            if (m.getAnnotation(DeriveMethod.class) != null) {

                Class<?> rtype = m.getReturnType();
                //				if( rtype.getSimpleName().compareTo(AbstractMetParameter.class.getName())  != 0  ) { // sanity check
                //					continue;
                //				}
                Class<?>[] deriveMthdArgs = m.getParameterTypes();

                // loop thru the list of args for this derive() method and check
                // if it is in the inputParams list. 
                for (Class<?> argClass : deriveMthdArgs) {
                    boolean prmFound = false;
                    boolean prmIsDerivable = false;

                    for (AbstractMetParameter inputPrm : inputParams) {

                        // if we have this input parameter or we can 
                        // derive this input parameter 
                        if (inputPrm.getClass() == argClass) {
                            prmFound = true;
                            break;
                        }
                    }

                    // if not in the list and if we have already checked this parameter 
                    // then see if it is derivable
                    // 
                    if (!prmFound && !checkedParams.contains(this.getMetParamName())) {

                        AbstractMetParameter argParam;
                        try {
                            argParam = (AbstractMetParameter) argClass.getConstructor().newInstance();

                            String metPrmName = argParam.getMetParamName();
                            checkedParams.add(metPrmName);

                            prmIsDerivable = argParam.derivable(checkedParams, inputParams);

                            if (prmIsDerivable) {
                                if (listOfInputMetPrmNamesForDerivingThisMetPrm == null)
                                    listOfInputMetPrmNamesForDerivingThisMetPrm = new ArrayList<String>(0);

                                listOfInputMetPrmNamesForDerivingThisMetPrm.add(metPrmName);
                            }
                            checkedParams.remove(metPrmName);

                        } catch (Exception e) {
                            System.out.println("error getting newInstance for metParam " + argClass.getSimpleName());
                        }
                    }

                    if (!prmFound && !prmIsDerivable) {
                        derivable = false;
                        //						break;
                    }
                } // end loop thru derive() args					

                if (derivable) {
                    foundDeriveMthds.add(m);
                }
            }
        }

        // 
        if (foundDeriveMthds.isEmpty()) {
            return null;
        } else if (foundDeriveMthds.size() > 1) {
            // If this happens then the caller should set the preferredDeriveParameters
            // list to tell this method which arguements to use.
            System.out.println("Sanity Check: metParameter " + getMetParamName() + " has multiple derive() methods for " + "the given input parameters.");
            return null;
        } else {
            return foundDeriveMthds.get(0);
        }
    }

    public AbstractMetParameter derive(Collection<AbstractMetParameter> inputParams) throws NotDerivableException {

        Method deriveMthd = getDeriveMethod(inputParams);

        if (deriveMthd == null) {
            setValueToMissing();
            return this;
            //		throw new NotDerivableException("can't derive param from given parameters.");
        }
        String errMsg = "";

        try {
            // check each derive method to see if its arguments are in the input 
            Class<?>[] deriveMthdArgs = deriveMthd.getParameterTypes();

            // a list of the parameter args (actual values) that will be passed to 
            // the derive() method. 
            List<AbstractMetParameter> mthdArgs = new ArrayList<AbstractMetParameter>(0);

            for (Class<?> argClass : deriveMthdArgs) {
                boolean prmFound = false;

                for (AbstractMetParameter inputPrm : inputParams) {
                    // if we don't have this input parameter, derive it  
                    // 
                    if (inputPrm.getClass() == argClass) {

                        if (!inputPrm.hasValidValue()) {
                            setValueToMissing();
                            return this;
                        } else {
                            mthdArgs.add(inputPrm);
                            prmFound = true;
                            break;
                        }
                    }
                }
                if (!prmFound) {
                    // create an object for this parameter and then set/derive 
                    // the value 
                    Constructor constr = argClass.getConstructor();
                    AbstractMetParameter derivedArgPrm = (AbstractMetParameter) constr.newInstance();
                    derivedArgPrm = derivedArgPrm.derive(inputParams);
                    mthdArgs.add(derivedArgPrm);
                }
            }

            Class<?> rtype = deriveMthd.getReturnType();
            Object derivedParam = null;

            switch (mthdArgs.size()) {
            case 1:
                derivedParam = deriveMthd.invoke(this, mthdArgs.get(0));
                break;
            case 2:
                derivedParam = deriveMthd.invoke(this, mthdArgs.get(0), mthdArgs.get(1));
                break;
            case 3:
                derivedParam = deriveMthd.invoke(this, mthdArgs.get(0), mthdArgs.get(1), mthdArgs.get(2));
                break;
            case 4:
                derivedParam = deriveMthd.invoke(this, mthdArgs.get(0), mthdArgs.get(1), mthdArgs.get(2), mthdArgs.get(3));
                break;
            }

            if (derivedParam != null) {
                if (listOfInputMetPrmNamesForDerivingThisMetPrm == null)
                    listOfInputMetPrmNamesForDerivingThisMetPrm = new ArrayList<String>(0);
                //            	 String metPrmName = derivedParam.getClass().getSimpleName();
                //            	 int size = mthdArgs.size();
                for (AbstractMetParameter thisPrm : mthdArgs) {
                    String metParamName = thisPrm.getMetParamName();
                    if (metParamName.compareTo(derivedParam.getClass().getSimpleName()) == 0) {
                        if (!listOfInputMetPrmNamesForDerivingThisMetPrm.contains(metParamName)) {
                            listOfInputMetPrmNamesForDerivingThisMetPrm.add(metParamName);
                        }
                    }
                }
                //            	 if(listOfInputMetPrmNamesForDerivingThisMetPrm.contains(metPrmName))
                //            		 listOfInputMetPrmNamesForDerivingThisMetPrm.remove(metPrmName);
            }

            return (AbstractMetParameter) derivedParam;

        } catch (IllegalArgumentException e) {
            errMsg = e.getMessage();
        } catch (IllegalAccessException e) {
            errMsg = e.getMessage();
        } catch (InvocationTargetException e) {
            errMsg = e.getMessage();
        } catch (SecurityException e) {
            errMsg = e.getMessage();
        } catch (NoSuchMethodException e) {
            errMsg = e.getMessage();
        } catch (InstantiationException e) {
            errMsg = e.getMessage();
        }

        throw new NotDerivableException(errMsg == null ? "" : errMsg);
    }

    public String getMetParamName() {
        return this.getClass().getSimpleName();
    }

    public ArrayList<String> getPreferedDeriveParameters() {
        return preferedDeriveParameters;
    }

    // Assume that the caller has already called isValidMetParameterName() to 
    // validate the names in the list.
    public void setPreferedDeriveParameters(ArrayList<String> preferedDeriveParameters) {
        this.preferedDeriveParameters = preferedDeriveParameters;
    }

    @Override
    public String toString() {
        //String 
        if (hasStringValue()) {
            return getClass().getSimpleName() + " " + getStringValue();
        } else {
            return getClass().getSimpleName() + " " + getValue().toString() + " " + getUnit().toString();
        }
    }

    //	@Override
    //	public Object clone(){
    //		AbstractMetParameter metParam = null;
    //		try {
    //			synchronized(this){
    //			metParam = this.getClass().newInstance();
    //			
    //			if(metParam == null )
    //				return metParam;
    //			
    //			if(this.getDataTime() == null )
    //				return metParam;
    //			if(this.getDataTime() != null )
    //			    metParam.dataTime = new DataTime(this.getDataTime().getRefTime());
    //			
    //			if( this.listOfInputMetPrmNamesForDerivingThisMetPrm != null )
    //			    metParam.listOfInputMetPrmNamesForDerivingThisMetPrm = new ArrayList<String>(this.listOfInputMetPrmNamesForDerivingThisMetPrm);
    //			
    //			if( this.valueString != null )
    //			    metParam.valueString = new String(this.valueString);
    //			
    //			if( this.getUnit() != null)
    //				metParam.setUnit(this.getUnit());
    //			
    //			if ( this.getUnitStr() != null )
    //				 metParam.setUnitStr( new String( this.getUnitStr() ) );
    //			
    //			if ( this.getValueString() != null )
    //				metParam.setValueString( new String( this.getValueString()));
    //			
    //			if ( this.getValue() != null  )
    //			     metParam.setValue(this.getValue());
    //				
    //			metParam.useStringValue = this.useStringValue;
    //			}	
    //			
    //		} catch (InstantiationException e) {
    //			// TODO Auto-generated catch block
    //			e.printStackTrace();
    //		} catch (IllegalAccessException e) {
    //			// TODO Auto-generated catch block
    //			e.printStackTrace();
    //		}
    //		
    //		return metParam;
    //	}

}
