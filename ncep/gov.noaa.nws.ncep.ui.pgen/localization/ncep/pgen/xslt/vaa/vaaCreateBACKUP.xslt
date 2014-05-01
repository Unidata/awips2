
<xsl:stylesheet version="1.0" 
	xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<xsl:template match="Volcano" mode="styleBACKUP">
<xsl:if test="@product='BACKUP'">
<xsl:text>
RMK: </xsl:text><xsl:value-of select="translate(@remarks, $smallcase, $uppercase)"/><xsl:text>&#13;&#10;</xsl:text>
</xsl:if>

</xsl:template>
</xsl:stylesheet> 
