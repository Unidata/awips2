<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:output method = "text"/>
<xsl:variable name="newline"><xsl:text>
 </xsl:text></xsl:variable>
<xsl:variable name="space"><xsl:text> </xsl:text></xsl:variable>

<!--
        wrapSfcText.xslt
                - Wrap the input string with line length charPerLine.
		  The line break will be put at the input character 'sep'.

        Change Log:

        B. Yin/Chugach  03/10   Initial Coding
-->

<xsl:template name="wrapSfcText">
        <xsl:param name="str"/>
        <xsl:param name="sep"/>
        <xsl:param name="charPerLine"/>

	<xsl:choose>
	   <xsl:when test="string-length($str) > $charPerLine">
		<xsl:choose>
			<!-- if the last char is sep, put a line break-->
			<xsl:when test="substring($str,$charPerLine + 1, 1) = $sep">
			
			    <!-- remove the last sep and replace other sep with space -->
				<xsl:variable name="oneLine">	
          		    <xsl:call-template name="replace">
						<xsl:with-param name="str" select="substring($str,1,$charPerLine)"/>
						<xsl:with-param name="old" select="$sep"/>
						<xsl:with-param name="new" select="$space"/>
				    </xsl:call-template>	
          		</xsl:variable>
          		
          		<xsl:value-of select="$oneLine"/>
          			<xsl:value-of select="$newline"/>
          			
				  <xsl:call-template name="wrapSfcText">
					<xsl:with-param name="str" select="substring($str,$charPerLine + 2)"/>
					<xsl:with-param name="charPerLine" select="$charPerLine"/>
					<xsl:with-param name="sep" select="$sep"/>
				  </xsl:call-template>
				
			</xsl:when>

			<!-- if the last char is not sep, search the last sep
			     then put a line break-->
			<xsl:otherwise>
				<xsl:variable name="line1">
					<xsl:call-template name="getLastSep">
						<xsl:with-param name="str" select="substring($str,1, $charPerLine)"/>
						<xsl:with-param name="sep" select="$sep"/>
					</xsl:call-template>
				</xsl:variable>
				
				<!-- remove the last sep and replace other sep with space -->
          		<xsl:variable name="line11">	
          		    <xsl:call-template name="replace">
						<xsl:with-param name="str" select="substring($line1,1, string-length($line1)-1)"/>
						<xsl:with-param name="old" select="$sep"/>
						<xsl:with-param name="new" select="$space"/>
				    </xsl:call-template>	
          		</xsl:variable>
          		
          		<xsl:value-of select="$line11"/>
          			<xsl:value-of select="$newline"/>
          				
				<xsl:call-template name="wrapSfcText">
					<xsl:with-param name="str" select="substring-after($str,$line1)"/>
					<xsl:with-param name="charPerLine" select="$charPerLine"/>
					<xsl:with-param name="sep" select="$sep"/>
				</xsl:call-template>
			</xsl:otherwise>
		</xsl:choose>
	    </xsl:when>
	    <xsl:otherwise>
          		<xsl:variable name="lastLine">
          		   <xsl:call-template name="replace">
						<xsl:with-param name="str" select="$str"/>
						<xsl:with-param name="old" select="$sep"/>
						<xsl:with-param name="new" select="$space"/>
				   </xsl:call-template>	
				</xsl:variable>
				
				<xsl:value-of select="$lastLine"/>
	    </xsl:otherwise>
	</xsl:choose>
</xsl:template>

<!--
       getLastSep 
                - get the string with last sep in the input string

        Change Log:

        B. Yin/Chugach  03/10   Initial Coding
-->
<xsl:template name="getLastSep">
        <xsl:param name="str"/>
        <xsl:param name="sep"/>
		<xsl:choose>
			<xsl:when test="contains($str,$sep)">	
				<xsl:value-of select="substring-before($str,$sep)"/>
				<xsl:value-of select="$sep"/>
				
				<xsl:variable name="newStr" select="substring-after($str,$sep)"/>

				<xsl:call-template name="getLastSep">
					<xsl:with-param name="str" select="$newStr"/>
					<xsl:with-param name="sep" select="$sep"/>
				</xsl:call-template>
			</xsl:when>
			<xsl:otherwise>
				<!--xsl:value-of select="$str"/-->
			</xsl:otherwise>
		</xsl:choose>
</xsl:template>

<!--
       replace 
                - replace the old string with the new string 

        Change Log:

        B. Yin/Chugach  03/10   Initial Coding
-->
<xsl:template name="replace">
        <xsl:param name="str"/>
        <xsl:param name="old"/>
        <xsl:param name="new"/>
		<xsl:choose>
			<xsl:when test="contains($str,$old)">	
				<xsl:value-of select="substring-before($str,$old)"/>
				<xsl:value-of select="$new"/>
				
				<xsl:variable name="newStr" select="substring-after($str,$old)"/>

				<xsl:call-template name="replace">
					<xsl:with-param name="str" select="$newStr"/>
					<xsl:with-param name="old" select="$old"/>
					<xsl:with-param name="new" select="$new"/>
				</xsl:call-template>
			</xsl:when>
			<xsl:otherwise>
				<xsl:value-of select="$str"/>
			</xsl:otherwise>
		</xsl:choose>
</xsl:template>
</xsl:stylesheet>
  
