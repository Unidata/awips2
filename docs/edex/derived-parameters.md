
AWIPS will calculate derived parameters using XML file definitions which refer to Python scripts where the actual calculations take place.   If and when there is an effort to verify calculated fields in Unidata Python packages, these should come in handy (along with the GEMPAK FORTRAN routines). 

For gridded data there are three directories to know which contain derived parm XML files:
awips2 com.raytheon.uf.common.dataplugin.grid https://github.com/Unidata/awips2/tree/unidata_16.1.5/edexOsgi/com.raytheon.uf.common.dataplugin.grid/utility/common_static/base/derivedParameters/definitions
awips2-core com.raytheon.uf.common.derivparam https://github.com/Unidata/awips2-core/tree/unidata_16.1.4/common/com.raytheon.uf.common.derivparam/utility/common_static/base/derivedParameters/definitions
awips2-core com.raytheon.uf.common.derivparam.python https://github.com/Unidata/awips2-core/tree/unidata_16.1.4/common/com.raytheon.uf.common.derivparam.python/utility/common_static/base/derivedParameters
Notice the first is from the "awips2" repo, while the others are from "awips2-core", so if a derived parm field is not showing up in directory listings or search results for one repo, be sure to search the other.

Helicity, for example, from https://github.com/Unidata/awips2-core/blob/unidata_16.1.4/common/com.raytheon.uf.common.derivparam/utility/common_static/base/derivedParameters/definitions/Heli.xml

<DerivedParameter unit="m*m/s*s" name="Helicity" abbreviation="Heli" xmlns:ns2="group">
	<Method name="Alias" levels="Surface">
		<Field abbreviation="srHel" level="Station"/>
	</Method>
	<Method name="Or" levels="0FHAG">
		<Field abbreviation="Heli" level="Surface"/>
		<Field abbreviation="Heli" level="BLyr"/>
	</Method>
    	<Method name="Alias" levels="0FHAG">
		<Field abbreviation="Heli" level="Surface"/>
	</Method> 
	<Method name="Heli" levels="0-1kmAgl>0-4kmAgl">
		<Field abbreviation="uWStk"/>
		<Field abbreviation="vWStk"/>
		<Field abbreviation="RM5" level="Layer"/>
	</Method>
</DerivedParameter>

The first three <Method></Method> blocks act as constructors allowing for different levels to be passed the main method, which determines the calculation to be performed. Name="Heli" here refers to the derivedParameter file Heli.py located at https://github.com/Unidata/awips2-core/blob/unidata_16.1.4/common/com.raytheon.uf.common.derivparam.python/utility/common_static/base/derivedParameters/functions/Heli.py

So three fields will be passed to Heli.py for the calculation (uWStk, vWStk, RM5), but each is a derived paremeter as well, with its own xml (let's go deeper). 
uWStk https://github.com/Unidata/awips2/blob/unidata_16.1.5/edexOsgi/com.raytheon.uf.common.dataplugin.grid/utility/common_static/base/derivedParameters/definitions/uStk.xml
<DerivedParameter abbreviation="uWStk" name="U Stack" unit="m/s">
    <Method name="Union" levels="0-3kmAgl">
    	<Field abbreviation="uW" level="0FHAG"/>
        <Field abbreviation="uW" level="0.5kmAgl"/>
        <Field abbreviation="uW" level="1kmAgl"/>
        <Field abbreviation="uW" level="1.5kmAgl"/>
        <Field abbreviation="uW" level="2kmAgl"/>
        <Field abbreviation="uW" level="2.5kmAgl"/>
        <Field abbreviation="uW" level="3kmAgl"/>
    </Method>
    <Method name="Union" levels="C,FHAG">
    	<Field abbreviation="uW"/>
    </Method>
</DerivedParameter>

vWStk https://github.com/Unidata/awips2/blob/unidata_16.1.5/edexOsgi/com.raytheon.uf.common.dataplugin.grid/utility/common_static/base/derivedParameters/definitions/vStk.xml
<DerivedParameter abbreviation="vWStk" name="V Stack" unit="m/s">
   <Method name="Union" levels="0-3kmAgl">
    	<Field abbreviation="vW" level="0FHAG"/>
        <Field abbreviation="vW" level="0.5kmAgl"/>
        <Field abbreviation="vW" level="1kmAgl"/>
        <Field abbreviation="vW" level="1.5kmAgl"/>
        <Field abbreviation="vW" level="2kmAgl"/>
        <Field abbreviation="vW" level="2.5kmAgl"/>
        <Field abbreviation="vW" level="3kmAgl"/>
    </Method>
    <Method name="Union" levels="C,FHAG">
    	<Field abbreviation="vW"/>
    </Method>
</DerivedParameter>
RM5 https://github.com/Unidata/awips2/blob/unidata_16.1.5/edexOsgi/com.raytheon.uf.common.dataplugin.grid/utility/common_static/base/derivedParameters/definitions/RM5.xml
<DerivedParameter name="Bunkers Right-Moving Supercell" unit="m/s" abbreviation="RM5" xmlns:ns2="group">
    <Method levels="Layer" name="Add">
        <Field level="0-6kmAgl" abbreviation="Wind"/>
        <Field abbreviation="RMprop"/>
    </Method>
     <Method name="Vector">
        <Field abbreviation="USTM"/>
        <Field abbreviation="VSTM"/>
    </Method>
</DerivedParameter>


Finally, the Python function Heli.py 
https://github.com/Unidata/awips2-core/blob/unidata_16.1.4/common/com.raytheon.uf.common.derivparam.python/utility/common_static/base/derivedParameters/functions/Heli.py

def execute(uStk, vStk, RM5):

    umot,vmot = RM5
    u1 = uStk[0]
    v1 = vStk[0]
    u2 = uStk[-1]
    v2 = vStk[-1]
    # First do our motion, lower bulk shear computation.
    hptr = (v2-v1)*umot+(u1-u2)*vmot
    for i in range(1, len(uStk)):
        u1 = uStk[i-1]
        v1 = vStk[i-1]
        u2 = uStk[i]
        v2 = vStk[i]
        hptr += u2*v1-u1*v2
    return hptr


from the three files above, we can see that the grid fields used to calculate helicity are uW, vW, USTM, and VSTM, with the last two used by yet another function Vector.py: https://github.com/Unidata/awips2-core/blob/unidata_16.1.4/common/com.raytheon.uf.common.derivparam.python/utility/common_static/base/derivedParameters/functions/Vector.py

Method names like Or, Union, and Alias that don't map to Python functions are handled by Java in com.raytheon.uf.common.derivparam (see https://github.com/Unidata/awips2-core/tree/unidata_16.1.4/common/com.raytheon.uf.common.derivparam/src/com/raytheon/uf/common/derivparam/tree)
