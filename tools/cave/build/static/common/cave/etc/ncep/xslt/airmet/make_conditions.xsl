<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
    
    <!-- 
        MakeConditionsStatement
        
        Output any of the possible conditions for a smear or outlook.  Both smears and
        outlooks can have a "conditions developing" statement and either a "conditions 
        continuing" or a "conditions ending" statement.
        
        Change Log
        ====== ===
        E. Safford/SAIC    04/06    initial coding
        E. Safford/SAIC    06/06    terminate each conditions statement with a period
    -->
    <xsl:template name="MakeConditionsStatement">
        <xsl:param name="isSmear">1</xsl:param>
        <xsl:param name="fromCondsDvlpg"/>
        <xsl:param name="fromCondsEndg"/>
        <xsl:param name="condsContg"/>
        <xsl:param name="otlkCondsDvlpg"/>
        <xsl:param name="otlkCondsEndg"/>
        
 
        <xsl:choose>
            <xsl:when test="contains( $isSmear, '1') ">                <!--  process smears  -->
                <xsl:if test="string-length( $fromCondsDvlpg) > 1 ">
                    <xsl:text> </xsl:text><xsl:value-of select="$fromCondsDvlpg"/><xsl:text>.</xsl:text>
                </xsl:if>
                
                <xsl:if test="string-length( $condsContg ) > 1">
                    <xsl:text> </xsl:text><xsl:value-of select="$condsContg"/><xsl:text>.</xsl:text>
                </xsl:if>
                              
                <xsl:if test="string-length( $fromCondsEndg ) > 1 and not( string-length( $condsContg ) > 1 )">
                    <xsl:text> </xsl:text><xsl:value-of select="$fromCondsEndg"/><xsl:text>.</xsl:text>
                </xsl:if>
            </xsl:when>
            
            <xsl:otherwise>                                                        <!--   process outlooks  -->
                <xsl:if test="string-length( $otlkCondsDvlpg ) > 1">
                    <xsl:text> </xsl:text><xsl:value-of select="$otlkCondsDvlpg"/><xsl:text>.</xsl:text>
                </xsl:if>
        
                <xsl:if test="string-length( $condsContg ) > 1">
                    <xsl:text> </xsl:text><xsl:value-of select="$condsContg"/><xsl:text>.</xsl:text>
                </xsl:if>
                  
                <xsl:if test="string-length( $otlkCondsEndg ) > 1 and not( string-length( $condsContg ) > 1 )">
                    <xsl:text> </xsl:text><xsl:value-of select="$otlkCondsEndg"/><xsl:text>.</xsl:text>
                </xsl:if>
            </xsl:otherwise>
            
        </xsl:choose>
        
    </xsl:template>
    
</xsl:stylesheet>
