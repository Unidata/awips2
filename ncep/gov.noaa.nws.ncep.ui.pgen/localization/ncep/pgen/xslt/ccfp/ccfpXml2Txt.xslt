<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" 
	xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  	<xsl:output method="text" encoding="UTF-8"/> 

	<xsl:include href="ccfpPart.xslt"/>
	
	<xsl:template match="DrawableElement">
	
		<xsl:text>CCFP  </xsl:text>  
		<xsl:value-of select="Sigmet/@editableAttrStartTime"/>
		<xsl:text>&#xA0;</xsl:text>
		<xsl:value-of select="Sigmet/@editableAttrEndTime"/>

<!-- Area is treated in one group and Line mixed with LineMed -->

		<xsl:for-each select="Sigmet[@type='Area']">
			<xsl:text>&#13;&#10;</xsl:text>
			<xsl:text>AREA  </xsl:text>
	    		<xsl:apply-templates select="."/>
		</xsl:for-each>

		<xsl:for-each select="Sigmet[@type!='Area']">
			<xsl:text>&#13;&#10;</xsl:text>
			<xsl:text>LINE  </xsl:text>
	    		<xsl:apply-templates select="."/>
		</xsl:for-each>
	       
	</xsl:template>

</xsl:stylesheet> 
