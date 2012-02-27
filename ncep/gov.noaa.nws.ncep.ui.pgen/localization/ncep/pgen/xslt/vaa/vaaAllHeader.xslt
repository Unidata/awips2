
<xsl:stylesheet version="1.0" 
	xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
	xmlns:datetime="http://exslt.org/dates-and-times"  
	xmlns:my="xalan://gov.noaa.nws.ncep.ui.pgen.sigmet.VaaInfo"     
    exclude-result-prefixes="datetime my">
    
    <xsl:variable name="smallcase" select="'abcdefghijklmnopqrstuvwxyz'" />
   	<xsl:variable name="uppercase" select="'ABCDEFGHIJKLMNOPQRSTUVWXYZ'" />
   	<xsl:variable name="now" select="datetime:date-time()"/>

<xsl:template match="Volcano" mode="styleHEADER">


<xsl:text>
FV</xsl:text><xsl:value-of select="@wmoId"/><xsl:value-of select="@hdrNum"/>
<xsl:text>&#xA0;</xsl:text>
<xsl:value-of select="substring(@origStnVAAC,1,4)"/>
<xsl:text>&#xA0;</xsl:text><!--xsl:value-of select="datetime:dateTime()" /--><xsl:value-of select="my:getDateTime('ddHHmm')" />
<xsl:text>&#xA0;</xsl:text>
<xsl:choose>
<xsl:when test="string-length(@corr) > 0">CC<xsl:value-of select="@corr" /></xsl:when>            
</xsl:choose>
VA ADVISORY<xsl:text>&#xA0;</xsl:text>
<xsl:choose>
<xsl:when test="string-length(@corr) > 0">-CORRECTION</xsl:when>            
</xsl:choose>
DTG: <!--xsl:value-of select="datetime:dateTime()"/--><xsl:value-of select="my:getDateTime('yyyyMMdd/HHmm')" />Z<xsl:text>&#13;&#10;</xsl:text>
VAAC: <xsl:value-of select="substring(@origStnVAAC,6)"/><xsl:text>&#13;&#10;</xsl:text> 
VOLCANO: <xsl:value-of select="translate(@name, $smallcase, $uppercase)"/><xsl:text>&#xA0;</xsl:text><xsl:value-of select="@number"/>
PSN: <xsl:choose><xsl:when test="@product='TEST'"><xsl:value-of select="@txtLoc"/></xsl:when><xsl:otherwise><xsl:value-of select="substring(@txtLoc,1,5)"/><xsl:text>&#xA0;</xsl:text><xsl:value-of select="substring(@txtLoc,6,6)"/></xsl:otherwise></xsl:choose> <xsl:text>&#13;&#10;</xsl:text> 
AREA: <xsl:value-of select="translate(@area, $smallcase, $uppercase)"/> <xsl:text>&#13;&#10;</xsl:text> 


</xsl:template>
</xsl:stylesheet> 
