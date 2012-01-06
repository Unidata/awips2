
<xsl:stylesheet version="1.0" 
	xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<xsl:template match="Volcano" mode="styleQUICK">

<xsl:if test="@product='QUICK'">

<xsl:text>
SUMMIT ELEV:  </xsl:text>
<xsl:value-of select="translate(@elev, $smallcase, $uppercase)"/><xsl:text>&#13;&#10;</xsl:text>
<xsl:text>
ADVISORY NR:  </xsl:text>
<xsl:value-of select="@year"/><xsl:if test="string-length(@year) > 0">/</xsl:if><xsl:value-of select="@advNum"/><xsl:text>&#13;&#10;</xsl:text>
INFO SOURCE: <xsl:value-of select="substring-after(@infoSource,':::')"/><xsl:text>&#xA0;</xsl:text><xsl:text>&#13;&#10;</xsl:text>           
ERUPTION DETAILS: <xsl:value-of select="translate(substring-after(@erupDetails,':::'), $smallcase, $uppercase)"/><xsl:text>&#13;&#10;</xsl:text>
OBS VA DTG: <xsl:choose><xsl:when test="@obsAshDate='NIL'">NIL</xsl:when><xsl:otherwise><xsl:value-of select="@obsAshDate"/>/<xsl:value-of select="@obsAshTime"/>Z</xsl:otherwise></xsl:choose><xsl:text>&#13;&#10;</xsl:text>  
OBS VA CLD: <xsl:value-of select="@obsFcstAshCloudInfo"/> <xsl:text>&#13;&#10;</xsl:text>
FCST VA CLD +6H: <xsl:value-of select="@obsFcstAshCloudInfo6"/> <xsl:text>&#13;&#10;</xsl:text>
FCST VA CLD +12H: <xsl:value-of select="@obsFcstAshCloudInfo12"/><xsl:text>&#13;&#10;</xsl:text>  
FCST VA CLD +18H: <xsl:value-of select="@obsFcstAshCloudInfo18"/> <xsl:text>&#13;&#10;</xsl:text>  

RMK:  <xsl:value-of select="substring-after(@remarks,':::')"/><xsl:text>&#13;&#10;</xsl:text>
NXT ADVISORY:   <xsl:value-of select="substring-after(@nextAdv,':::')"/><xsl:text>&#13;&#10;</xsl:text>  

</xsl:if>

</xsl:template>
</xsl:stylesheet> 
