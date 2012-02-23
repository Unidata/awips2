
<xsl:stylesheet version="1.0" 
	xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<xsl:template match="Volcano" mode="styleRESUME">
<xsl:if test="@product='RESUME'">
<xsl:text>
RMK: </xsl:text><xsl:value-of select="substring-after(@remarks,':::')"/><xsl:text>&#13;&#10;</xsl:text>
</xsl:if>

</xsl:template>
</xsl:stylesheet> 
