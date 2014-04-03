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
 * Subscription.xsl
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
    xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" version="1.0">
    <xsl:import
        href="./com.raytheon.uf.common.datadelivery.registry.Time.xsl" />
    <xsl:import
        href="./com.raytheon.uf.common.datadelivery.registry.Parameter.xsl" />
    <xsl:import
        href="./com.raytheon.uf.common.datadelivery.registry.Ensemble.xsl" />
    <xsl:import
        href="./com.raytheon.uf.common.datadelivery.registry.Coverage.xsl" />
    <xsl:output method="xml" encoding="utf-8" indent="yes" />
    <xsl:template name="subscription">

        <xsl:if test="@owner != ''">
            <xsl:attribute name="owner">
    			<xsl:value-of select="@owner" />
			</xsl:attribute>
        </xsl:if>
        <xsl:if test="@subscriptionState != ''">
            <xsl:attribute name="subscriptionState">
        		<xsl:value-of select="@subscriptionState" />
			</xsl:attribute>
        </xsl:if>
        <xsl:if test="@subscriptionType != ''">
            <xsl:attribute name="subscriptionType">
        		<xsl:value-of select="@subscriptionType" />
			</xsl:attribute>
        </xsl:if>
        <xsl:if test="@originatingSite != ''">
            <xsl:attribute name="originatingSite">
        		<xsl:value-of select="@originatingSite" />
			</xsl:attribute>
        </xsl:if>
        <xsl:if test="@latencyInMinutes != ''">
            <xsl:attribute name="latencyInMinutes">
        		<xsl:value-of select="@latencyInMinutes" />
			</xsl:attribute>
        </xsl:if>
        <xsl:if test="@route != ''">
            <xsl:attribute name="route">
        		<xsl:value-of select="@route" />
			</xsl:attribute>
        </xsl:if>
        <xsl:if test="@description != ''">
            <xsl:attribute name="description">
        		<xsl:value-of select="@description" />
			</xsl:attribute>
        </xsl:if>
        <xsl:if test="@dataSetSize != ''">
            <xsl:attribute name="dataSetSize">
        		<xsl:value-of select="@dataSetSize" />
			</xsl:attribute>
        </xsl:if>
        <xsl:if test="@fullDataSet != ''">
            <xsl:attribute name="fullDataSet">
        		<xsl:value-of select="@fullDataSet" />
			</xsl:attribute>
        </xsl:if>
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
        <xsl:if test="@priority != ''">
            <xsl:attribute name="priority">
        		<xsl:value-of select="@priority" />
			</xsl:attribute>
        </xsl:if>
        <xsl:if test="@provider != ''">
            <xsl:attribute name="provider">
        		<xsl:value-of select="@provider" />
			</xsl:attribute>
        </xsl:if>
        <xsl:if test="@groupName != ''">
            <xsl:attribute name="groupName">
        		<xsl:value-of select="@groupName" />
			</xsl:attribute>
        </xsl:if>
        <xsl:if test="@name != ''">
            <xsl:attribute name="name">
        		<xsl:value-of select="@name" />
			</xsl:attribute>
        </xsl:if>
        <xsl:if test="@subscriptionId != ''">
            <xsl:attribute name="subscriptionId">
        		<xsl:value-of select="@subscriptionId" />
			</xsl:attribute>
        </xsl:if>
        <xsl:if test="@deleted != ''">
            <xsl:attribute name="deleted">
        		<xsl:value-of select="@deleted" />
			</xsl:attribute>
        </xsl:if>
        <xsl:if test="@id != ''">
            <xsl:attribute name="id">
        		<xsl:value-of select="@id" />
			</xsl:attribute>
        </xsl:if>
        <xsl:if test="@subscriptionId != ''">
            <xsl:attribute name="subscriptionId">
        		<xsl:value-of select="@subscriptionId" />
			</xsl:attribute>
        </xsl:if>
        <xsl:if test="@dataSetType != ''">
            <xsl:attribute name="dataSetType">
        		<xsl:value-of select="@dataSetType" />
			</xsl:attribute>
        </xsl:if>
        <xsl:if test="@url != ''">
            <xsl:attribute name="url">
        		<xsl:value-of select="@url" />
			</xsl:attribute>
        </xsl:if>
        <xsl:if test="@unscheduled != ''">
            <xsl:attribute name="unscheduled">
        		<xsl:value-of select="@unscheduled" />
			</xsl:attribute>
        </xsl:if>
        <xsl:if test="@dataSetName != ''">
            <xsl:attribute name="dataSetName">
        		<xsl:value-of select="@dataSetName" />
			</xsl:attribute>
        </xsl:if>
        <xsl:if test="@valid != ''">
            <xsl:attribute name="valid">
        		<xsl:value-of select="@valid" />
			</xsl:attribute>
        </xsl:if>
        <xsl:if test="@description != ''">
            <xsl:attribute name="description">
        		<xsl:value-of select="@description" />
			</xsl:attribute>
        </xsl:if>

        <xsl:for-each select="officeId">
            <xsl:element name="officeId">
                <xsl:value-of select="." />
            </xsl:element>
        </xsl:for-each>

        <!-- List of applied templates -->
        <xsl:apply-templates select="coverage" />
        <xsl:apply-templates select="time" />
        <xsl:if test="ensemble">
            <xsl:apply-templates select="ensemble" />
        </xsl:if>

        <xsl:for-each select="parameter">
            <xsl:call-template name="bareParameter" />
        </xsl:for-each>

    </xsl:template>

</xsl:stylesheet>