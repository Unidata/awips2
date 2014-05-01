<?xml version="1.0" encoding="UTF-8" ?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
 <xsl:output method="text"/>

    <!--
        indent.xsl

        This is an xsl template document.  It is used to output an xml document
        that conforms to the indent.xsd schema, and outputs it as text.  The
        text is left justified or indented from the left according to each
        line element's indent attribute.  Indentation can be from 1 to 20 spaces.
        If any line element has no indent attribute, or if the indent 
	attribute exceeds 20, then the line will output as fully left justified
        (0 space indentation).

        This is designed to be the second stage in a 2 stage output process.
        Xslt is great at parsing a document and converting it to xml or xslt.  
        But text output, particularly if individual spacing is important, is
        problematic.  It's easier if stage 1 processing builds the line by line
        content and the second stage worries about text formating.

        Change Log
        
        E. Safford/SAIC		03/05	initial coding
    -->

        

  <xsl:template match="/">
     <xsl:for-each select="//line">
        <xsl:variable name="myline"  select="."/>
        <xsl:variable name="space" select="@indent"/>

	<xsl:choose>
	   <xsl:when test="$space = 0">
<xsl:value-of select="$myline"/><xsl:text>
</xsl:text>
	   </xsl:when>
	   <xsl:when test="$space = 1">
<xsl:text> </xsl:text><xsl:value-of select="$myline"/><xsl:text>
</xsl:text>
	   </xsl:when>
	   <xsl:when test="$space = 2">
<xsl:text>  </xsl:text><xsl:value-of select="$myline"/><xsl:text>
</xsl:text>
	   </xsl:when>
	   <xsl:when test="$space = 3">
<xsl:text>   </xsl:text><xsl:value-of select="$myline"/><xsl:text>
</xsl:text>
	   </xsl:when>
	   <xsl:when test="$space = 4">
<xsl:text>    </xsl:text><xsl:value-of select="$myline"/><xsl:text>
</xsl:text>
	   </xsl:when>
	   <xsl:when test="$space = 5">
<xsl:text>     </xsl:text><xsl:value-of select="$myline"/><xsl:text>
</xsl:text>
	   </xsl:when>
	   <xsl:when test="$space = 6">
<xsl:text>      </xsl:text><xsl:value-of select="$myline"/><xsl:text>
</xsl:text>
	   </xsl:when>
	   <xsl:when test="$space = 7">
<xsl:text>       </xsl:text><xsl:value-of select="$myline"/><xsl:text>
</xsl:text>
	   </xsl:when>
	   <xsl:when test="$space = 8">
<xsl:text>        </xsl:text><xsl:value-of select="$myline"/><xsl:text>
</xsl:text>
	   </xsl:when>
	   <xsl:when test="$space = 9">
<xsl:text>         </xsl:text><xsl:value-of select="$myline"/><xsl:text>
</xsl:text>
	   </xsl:when>
	   <xsl:when test="$space = 10">
<xsl:text>          </xsl:text><xsl:value-of select="$myline"/><xsl:text>
</xsl:text>
	   </xsl:when>
	   <xsl:when test="$space = 11">
<xsl:text>           </xsl:text><xsl:value-of select="$myline"/><xsl:text>
</xsl:text>
	   </xsl:when>
	   <xsl:when test="$space = 12">
<xsl:text>            </xsl:text><xsl:value-of select="$myline"/><xsl:text>
</xsl:text>
	   </xsl:when>
	   <xsl:when test="$space = 13">
<xsl:text>             </xsl:text><xsl:value-of select="$myline"/><xsl:text>
</xsl:text>
	   </xsl:when>
	   <xsl:when test="$space = 14">
<xsl:text>              </xsl:text><xsl:value-of select="$myline"/><xsl:text>
</xsl:text>
	   </xsl:when>
	   <xsl:when test="$space = 15">
<xsl:text>               </xsl:text><xsl:value-of select="$myline"/><xsl:text>
</xsl:text>
	   </xsl:when>
	   <xsl:when test="$space = 16">
<xsl:text>                </xsl:text><xsl:value-of select="$myline"/><xsl:text>
</xsl:text>
	   </xsl:when>
	   <xsl:when test="$space = 17">
<xsl:text>                 </xsl:text><xsl:value-of select="$myline"/><xsl:text>
</xsl:text>
	   </xsl:when>
	   <xsl:when test="$space = 18">
<xsl:text>                  </xsl:text><xsl:value-of select="$myline"/><xsl:text>
</xsl:text>
	   </xsl:when>
	   <xsl:when test="$space = 19">
<xsl:text>                   </xsl:text><xsl:value-of select="$myline"/><xsl:text>
</xsl:text>
	   </xsl:when>
	   <xsl:when test="$space = 20">
<xsl:text>                    </xsl:text><xsl:value-of select="$myline"/><xsl:text>
</xsl:text>
	   </xsl:when>
           <xsl:otherwise>
<xsl:value-of select="$myline"/><xsl:text>
</xsl:text>	    
	   </xsl:otherwise>
	</xsl:choose>

     </xsl:for-each>    
  </xsl:template>

</xsl:stylesheet>
