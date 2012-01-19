<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" 
	xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  	<xsl:output method="text" encoding="UTF-8"/> 
  
	<xsl:include href="vaaAllHeader.xslt"/>
	
	<xsl:include href="vaaCreateTEST.xslt"/>
	<xsl:include href="vaaCreateRESUME.xslt"/>
	<xsl:include href="vaaCreateBACKUP.xslt"/>
	<xsl:include href="vaaEditNORMAL.xslt"/>
	<xsl:include href="vaaEditEND.xslt"/>
	<xsl:include href="vaaEditQUICK.xslt"/>
	<xsl:include href="vaaEditNEAR.xslt"/>
	<xsl:include href="vaaEditWATCH.xslt"/>
	
	<xsl:include href="vaaAllFooter.xslt"/>
	
	<xsl:template match="Volcano">
	
		<xsl:apply-templates select="." mode="styleHEADER" />	
		 
	    <xsl:apply-templates select="." mode="styleTEST" />
	    <xsl:apply-templates select="." mode="styleRESUME" />
	    <xsl:apply-templates select="." mode="styleBACKUP" />
	    <xsl:apply-templates select="." mode="styleNORMAL" />
	    <xsl:apply-templates select="." mode="styleEND" />
	    <xsl:apply-templates select="." mode="styleQUICK" />
	    <xsl:apply-templates select="." mode="styleNEAR" />
	    <xsl:apply-templates select="." mode="styleWATCH" />
	    
	    
	    <xsl:apply-templates select="." mode="styleFOOTER" />  
	       
	</xsl:template>

</xsl:stylesheet> 
