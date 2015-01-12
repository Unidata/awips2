<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:output method = "text"/>

<xsl:import href="wrapSfcText.xslt"/>

<!--
	getSfcHighsLows.xslt
		- generate formatted highs/lows for SFC_PROG text	

	Change Log:

	B. Yin/SGT		02/14	Initial Coding
-->

<xsl:variable name="newline"><xsl:text>
</xsl:text></xsl:variable>

<xsl:template name="getSfcHighsLows">
        <xsl:param name="latFmt"/>
        <xsl:param name="lonFmt"/>

 <xsl:variable name="highs"> 
    <xsl:text>HIGHS </xsl:text>
	<!--High symbol with labels -->
    <xsl:for-each select="/Products/Product/Layer/DrawableElement/DECollection">
    	<xsl:if test="@pgenCategory= 'Symbol' and (@pgenType= 'HIGH_PRESSURE_H' or @pgenType ='FILLED_HIGH_PRESSURE_H')">
              <xsl:call-template name="fmtText">
				<xsl:with-param name="text" select="DrawableElement/Text/textLine"/>
				<xsl:with-param name="lat" select="DrawableElement/Symbol/Point/@Lat"/>
				<xsl:with-param name="lon" select="DrawableElement/Symbol/Point/@Lon"/>
				<xsl:with-param name="latFmt" select="$latFmt"/>
				<xsl:with-param name="lonFmt" select="$lonFmt"/>
			</xsl:call-template>
        </xsl:if>
    </xsl:for-each>
    
	<!--High symbol without labels -->
   <xsl:for-each select="/Products/Product/Layer/DrawableElement/Symbol">
    	<xsl:if test="@pgenCategory= 'Symbol' and (@pgenType= 'HIGH_PRESSURE_H' or @pgenType ='FILLED_HIGH_PRESSURE_H')">
           <xsl:call-template name="fmtText">
				<xsl:with-param name="text" select="DrawableElement/Text/textLine"/>
				<xsl:with-param name="lat" select="Point/@Lat"/>
				<xsl:with-param name="lon" select="Point/@Lon"/>
				<xsl:with-param name="latFmt" select="$latFmt"/>
				<xsl:with-param name="lonFmt" select="$lonFmt"/>
			</xsl:call-template>   
              
        </xsl:if>
    </xsl:for-each>
    
	<!--High symbol in a contours -->
    <xsl:for-each select="/Products/Product/Layer/DrawableElement/Contours/DECollection">
    	<xsl:if test="@collectionName= 'ContourMinmax' and (DrawableElement/Symbol/@pgenType = 'HIGH_PRESSURE_H' or DrawableElement/Symbol/@pgenType = 'FILLED_HIGH_PRESSURE_H')">
	     <xsl:call-template name="fmtText">
				<xsl:with-param name="text" select="DrawableElement/Text/textLine"/>
				<xsl:with-param name="lat" select="DrawableElement/Symbol/Point/@Lat"/>
				<xsl:with-param name="lon" select="DrawableElement/Symbol/Point/@Lon"/>
				<xsl:with-param name="latFmt" select="$latFmt"/>
				<xsl:with-param name="lonFmt" select="$lonFmt"/>
			</xsl:call-template>
        </xsl:if>
    </xsl:for-each>
</xsl:variable>

<xsl:call-template name="wrapSfcText">
	<xsl:with-param name="str" select="$highs"/>
	<xsl:with-param name="charPerLine" select="80"/>
	<xsl:with-param name="sep" select="';'"/>
</xsl:call-template>
		       
   <xsl:value-of select="$newline"/>
   
 <xsl:variable name="lows">    
   <xsl:text>LOWS </xsl:text>

<!--Low symbol with labels -->
    <xsl:for-each select="/Products/Product/Layer/DrawableElement/DECollection">
    	<xsl:if test="@pgenCategory= 'Symbol' and (@pgenType= 'LOW_PRESSURE_L' or @pgenType ='FILLED_LOW_PRESSURE_L')">
              <xsl:call-template name="fmtText">
				<xsl:with-param name="text" select="DrawableElement/Text/textLine"/>
				<xsl:with-param name="lat" select="DrawableElement/Symbol/Point/@Lat"/>
				<xsl:with-param name="lon" select="DrawableElement/Symbol/Point/@Lon"/>
				<xsl:with-param name="latFmt" select="$latFmt"/>
				<xsl:with-param name="lonFmt" select="$lonFmt"/>
			</xsl:call-template>
        </xsl:if>
    </xsl:for-each>
   
 <!--Low symbol without labels -->
	<xsl:for-each select="/Products/Product/Layer/DrawableElement/Symbol">
    	<xsl:if test="@pgenCategory= 'Symbol' and (@pgenType= 'LOW_PRESSURE_L' or @pgenType ='FILLED_LOW_PRESSURE_L')">
           <xsl:call-template name="fmtText">
				<xsl:with-param name="text" select="DrawableElement/Text/textLine"/>
				<xsl:with-param name="lat" select="Point/@Lat"/>
				<xsl:with-param name="lon" select="Point/@Lon"/>
				<xsl:with-param name="latFmt" select="$latFmt"/>
				<xsl:with-param name="lonFmt" select="$lonFmt"/>
			</xsl:call-template>   
        </xsl:if>
    </xsl:for-each>
    
<!--Low symbol in contours -->
    <xsl:for-each select="/Products/Product/Layer/DrawableElement/Contours/DECollection">
    	<xsl:if test="@collectionName= 'ContourMinmax' and (DrawableElement/Symbol/@pgenType = 'LOW_PRESSURE_L' or DrawableElement/Symbol/@pgenType = 'FILLED_LOW_PRESSURE_L')">
	     <xsl:call-template name="fmtText">
				<xsl:with-param name="text" select="DrawableElement/Text/textLine"/>
				<xsl:with-param name="lat" select="DrawableElement/Symbol/Point/@Lat"/>
				<xsl:with-param name="lon" select="DrawableElement/Symbol/Point/@Lon"/>
				<xsl:with-param name="latFmt" select="$latFmt"/>
				<xsl:with-param name="lonFmt" select="$lonFmt"/>
			</xsl:call-template>
        </xsl:if>
    </xsl:for-each>
 </xsl:variable>

 <xsl:call-template name="wrapSfcText">
	<xsl:with-param name="str" select="$lows"/>
	<xsl:with-param name="charPerLine" select="80"/>
	<xsl:with-param name="sep" select="';'"/>
</xsl:call-template>
      
  </xsl:template>
  
  <xsl:template name="fmtText">
        <xsl:param name="text"/>
        <xsl:param name="lat"/>
        <xsl:param name="lon"/>
        <xsl:param name="latFmt"/>
        <xsl:param name="lonFmt"/>
          
          <xsl:value-of select="$text"/>
          <xsl:text> </xsl:text>
	      <xsl:value-of select="format-number($lat,$latFmt)"/>
	      <xsl:value-of select="format-number($lon*-1,$lonFmt)"/>
          <xsl:text>;</xsl:text>
    </xsl:template>
  
</xsl:stylesheet>
