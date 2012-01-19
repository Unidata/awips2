<xsl:stylesheet version="1.0"
 xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
 xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
>

<!-- bundleToMenu.xslt transforms a saved bundle into a menu ready 
     bundle by removing some redundant and unneeded information -->

<!-- copy everything from the bundle -->
 <xsl:output omit-xml-declaration="yes"/>

    <xsl:template match="node()|@*">
      <xsl:copy>
         <xsl:apply-templates select="node()|@*"/>
      </xsl:copy>
    </xsl:template>

<!-- remove the information saved in a bundle that isn't needed for menus -->
<xsl:template match="gridGeometry|limitedNumberOfFrames|numberOfFrames"/>
<xsl:template match="capability[@xsi:type='colorableCapability']"/>
<xsl:template match="//resource[resourceData/@xsi:type='mapResourceGroupData']"/>

</xsl:stylesheet>
