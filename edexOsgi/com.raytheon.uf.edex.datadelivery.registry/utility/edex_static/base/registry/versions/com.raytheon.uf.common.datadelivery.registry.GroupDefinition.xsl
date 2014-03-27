<?xml version="1.0" encoding="ISO-8859-1"?>
<?xml version="1.0" encoding="ISO-8859-1"?>
<!-- 
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 * 
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 * 
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 * 
 * GropupDefinition.xsl
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 ******************************************************************
 * Mar 02, 2014 2789      dhladky      XSLT transformation of other versions to current.
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
-->


<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:ns2="com.raytheon.uf.common.datadelivery.registry" version="1.0">
    <xsl:output method="xml" encoding="utf-8" indent="yes" />

    <xsl:template match="groupDefinition" name="groupDefinition">

        <groupDefinition xmlns:ns2="com.raytheon.uf.common.datadelivery.registry"
            xmlns:ns4="urn:oasis:names:tc:ebxml-regrep:xsd:rs:4.0"
            xmlns:ns3="urn:oasis:names:tc:ebxml-regrep:xsd:rim:4.0"
            xmlns:ns9="urn:oasis:names:tc:ebxml-regrep:xsd:lcm:4.0"
            xmlns:ns5="http://www.w3.org/1999/xlink" xmlns:ns6="urn:oasis:names:tc:ebxml-regrep:xsd:spi:4.0"
            xmlns:ns7="http://www.w3.org/2005/08/addressing" xmlns:ns8="urn:oasis:names:tc:ebxml-regrep:xsd:query:4.0">

            <xsl:if test="@subscriptionEnd != ''">
                <xsl:attribute name="subscriptionEnd">
        			<xsl:value-of select="@subscriptionEnd" />
				</xsl:attribute>
            </xsl:if>
            <xsl:if test="@subscriptionStart != ''">
                <xsl:attribute name="subscriptionStart">
        			<xsl:value-of select="@subscriptionStart" />
				</xsl:attribute>
            </xsl:if>
            <xsl:if test="@activePeriodEnd != ''">
                <xsl:attribute name="activePeriodEnd">
        			<xsl:value-of select="@activePeriodEnd" />
				</xsl:attribute>
            </xsl:if>
            <xsl:if test="@activePeriodStart != ''">
                <xsl:attribute name="activePeriodStart">
        			<xsl:value-of select="@activePeriodStart" />
				</xsl:attribute>
            </xsl:if>
            <xsl:if test="@owner != ''">
                <xsl:attribute name="owner">
        			<xsl:value-of select="@owner" />
				</xsl:attribute>
            </xsl:if>
            <xsl:if test="@groupName != ''">
                <xsl:attribute name="groupName">
        			<xsl:value-of select="@groupName" />
				</xsl:attribute>
            </xsl:if>

            <xsl:if test="envelope">
                <xsl:element name="envelope">
                    <xsl:if test="@maxY != ''">
                        <xsl:attribute name="maxY">
        					<xsl:value-of select="@maxY" />
						</xsl:attribute>
                    </xsl:if>
                    <xsl:if test="@maxY != ''">
                        <xsl:attribute name="minY">
        					<xsl:value-of select="@minY" />
						</xsl:attribute>
                    </xsl:if>
                    <xsl:if test="@maxX != ''">
                        <xsl:attribute name="maxX">
        					<xsl:value-of select="@maxX" />
						</xsl:attribute>
                    </xsl:if>
                    <xsl:if test="@minX != ''">
                        <xsl:attribute name="minX">
        					<xsl:value-of select="@minX" />
						</xsl:attribute>
                    </xsl:if>
                    <xsl:if test="crs != ''">
                        <xsl:element name="crs">
                            <xsl:value-of select="crs" />
                        </xsl:element>
                    </xsl:if>
                </xsl:element>
            </xsl:if>
        </groupDefinition>
    </xsl:template>

</xsl:stylesheet>