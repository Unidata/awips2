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
 * Provider.xsl
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
    version="1.0">
    <xsl:import
        href="./com.raytheon.uf.common.datadelivery.registry.Connection.xsl" />
    <xsl:import
        href="./com.raytheon.uf.common.datadelivery.registry.Projection.xsl" />
    <xsl:output method="xml" encoding="utf-8" indent="yes" />
    <xsl:template match="provider" name="provider">

        <provider xmlns:ns2="http://www.w3.org/1999/xlink"
            xmlns:ns4="urn:oasis:names:tc:ebxml-regrep:xsd:rs:4.0"
            xmlns:ns3="http://www.w3.org/2005/08/addressing" xmlns:ns9="com.raytheon.uf.common.datadelivery.registry"
            xmlns:ns5="urn:oasis:names:tc:ebxml-regrep:xsd:query:4.0"
            xmlns:ns6="urn:oasis:names:tc:ebxml-regrep:xsd:spi:4.0"
            xmlns:ns7="urn:oasis:names:tc:ebxml-regrep:xsd:lcm:4.0"
            xmlns:ns8="urn:oasis:names:tc:ebxml-regrep:xsd:rim:4.0">

            <xsl:if test="@serviceType != ''">
                <xsl:attribute name="serviceType">
        			<xsl:value-of select="@serviceType" />
    			</xsl:attribute>
            </xsl:if>
            <xsl:if test="@name != ''">
                <xsl:attribute name="name">
        			<xsl:value-of select="@name" />
   				</xsl:attribute>
            </xsl:if>

            <!-- List of applied templates -->
            <xsl:if test="connection">
                <xsl:apply-templates select="connection" />
            </xsl:if>
            <xsl:if test="projection">
                <xsl:apply-templates select="projection" />
            </xsl:if>
            <xsl:for-each select="providerType">
                <xsl:call-template name="providerType" />
            </xsl:for-each>
            <xsl:if test="postedFileDelay">
                <xsl:element name="postedFileDelay">
                    <xsl:value-of select="postedFileDelay" />
                </xsl:element>
            </xsl:if>
            <xsl:if test="timeBetweenCrawlRequests">
                <xsl:element name="timeBetweenCrawlRequests">
                    <xsl:value-of select="timeBetweenCrawlRequests" />
                </xsl:element>
            </xsl:if>
        </provider>
    </xsl:template>

    <xsl:template match="providerType" name="providerType">
        <xsl:element name="providerType">
            <xsl:if test="@availabilityDelay != ''">
                <xsl:attribute name="availabilityDelay">
        			<xsl:value-of select="@availabilityDelay" />
    			</xsl:attribute>
            </xsl:if>
            <xsl:if test="@plugin != ''">
                <xsl:attribute name="plugin">
        			<xsl:value-of select="@plugin" />
    			</xsl:attribute>
            </xsl:if>
            <xsl:if test="@dataType != ''">
                <xsl:attribute name="dataType">
        			<xsl:value-of select="@dataType" />
    			</xsl:attribute>
            </xsl:if>
        </xsl:element>
    </xsl:template>

</xsl:stylesheet>