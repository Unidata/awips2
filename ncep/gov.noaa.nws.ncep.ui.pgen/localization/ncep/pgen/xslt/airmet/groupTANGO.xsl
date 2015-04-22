<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
  <xsl:output method="xml" version="1.0" encoding="iso-8859-1" indent="yes"/>
  <xsl:strip-space elements="*"/>
  <xsl:template match="bulletin">
    <xsl:for-each select="productValidTime">
      <xsl:copy-of select="."/>
    </xsl:for-each>
    <xsl:for-each select="gfaObject[@type='TURB-HI']">
      <xsl:copy-of select="."/>
    </xsl:for-each>
    <xsl:for-each select="gfaObject[@type='TURB-LO']">
      <xsl:copy-of select="."/>
    </xsl:for-each>
    <xsl:for-each select="gfaObject[@type='SFC_WND']">
      <xsl:copy-of select="."/>
    </xsl:for-each>
    <xsl:for-each select="gfaObject[@type='LLWS']">
      <xsl:copy-of select="."/>
    </xsl:for-each>
  </xsl:template>
  <xsl:template match="/">
    <gfaInfo>
      <bulletin type="TANGO">
        <xsl:apply-templates/>
      </bulletin>
    </gfaInfo>    
  </xsl:template> 
</xsl:stylesheet>

