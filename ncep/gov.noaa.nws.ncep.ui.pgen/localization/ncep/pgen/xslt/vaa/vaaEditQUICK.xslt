
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
INFO SOURCE: <xsl:if test="not(contains( @infoSource, 'NOT AVBL'))" ><xsl:value-of select="substring-after(@infoSource,':::')"/><xsl:text>&#xA0;</xsl:text></xsl:if><xsl:text>&#13;&#10;</xsl:text>          
ERUPTION DETAILS: <xsl:if test="not(contains( @erupDetails, 'NOT AVBL'))" ><xsl:value-of select="translate(substring-after(@erupDetails,':::'), $smallcase, $uppercase)"/><xsl:text>&#13;&#10;</xsl:text></xsl:if>

RMK:  <xsl:value-of select="substring-after(@remarks,':::')"/><xsl:text>&#13;&#10;</xsl:text>
NXT ADVISORY:   <xsl:value-of select="substring-after(@nextAdv,':::')"/><xsl:text>&#13;&#10;</xsl:text>  

</xsl:if>

</xsl:template>
</xsl:stylesheet> 
