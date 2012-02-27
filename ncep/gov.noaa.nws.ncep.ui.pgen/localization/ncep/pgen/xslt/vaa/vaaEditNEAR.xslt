
<xsl:stylesheet version="1.0" 
	xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<xsl:template match="Volcano" mode="styleNEAR">
<xsl:if test="@product='NEAR'">

<xsl:text>
SUMMIT ELEV:  </xsl:text>
<xsl:value-of select="translate(@elev, $smallcase, $uppercase)"/><xsl:text>&#13;&#10;</xsl:text>
<xsl:text>
ADVISORY NR:  </xsl:text> 
<xsl:value-of select="@year"/><xsl:if test="string-length(@year) > 0">/</xsl:if><xsl:value-of select="@advNum"/><xsl:text>&#13;&#10;</xsl:text>
RMK:  <xsl:value-of select="translate(@remarks, $smallcase, $uppercase)"/>  ...<xsl:value-of select="@forecasters"/><xsl:text>&#13;&#10;</xsl:text>

</xsl:if>

</xsl:template>
</xsl:stylesheet> 
