
Data Delivery has been implemented into the AWIPS(II) baseline to provide access to data that is not resident locally at a Weather Forecast Office, River Forecast Center, or National Center. Data Delivery gives users the ability to create queries (One Time Requests) and
subscriptions to data sets (provided OGC / OpenDAP servers such as THREDDS).

# build.edex/build.xml

    <target name="main-build" depends="clean">
    		<antcall target="build">
    			<param name="feature"
    				value="com.raytheon.uf.common.base.feature" />
    		</antcall>
    		<antcall target="build">
    			<param name="feature"
    				value="com.raytheon.uf.edex.base.feature" />
    		</antcall>
    ...
            <antcall target="build">
    			<param name="feature"
    				value="gov.nasa.msfc.sport.edex.glmdecoder.feature" />
    		</antcall>
    		<!--
                <antcall target="build">
                    <param name="feature"
                        value="com.raytheon.uf.edex.datadelivery.feature" />
                </antcall>
                <antcall target="build">
                    <param name="feature"
                        value="com.raytheon.uf.edex.ogc.feature" />
                </antcall>
    		-->
    	</target>
    	
Notice the last two commented out, `com.raytheon.uf.edex.datadelivery.feature` and `com.raytheon.uf.edex.ogc.feature`.  These feature sets *do not exist*, but could easily be created in the same wat as other features (like `com.raytheon.uf.common.base.feature`, `com.raytheon.uf.edex.base.feature`, etc.

## wa-build

The source code comments provide the following guidance:

> In the work assignment's edexOsgi/build.edex directory, create a file named similar to the following:
edexOsgi/build.edex/5-Data_Delivery-wa-build.properties In the file, there should be one line such as:
wa.features=feature1,feature2

However, the `wa-build` Ant target requires a file `features.txt` exist.  So if is `5-Data_Delivery-wa-build.properties` or `features.txt`?  Because the delimiter being specified is a line separator (and not a comma "wa.features=feature1,feature2" as with versions proir to 16.2.2).

So we can infer that a file should exist called features.txt should exist which has one WA feature per line.  And what do you know, a similar file exist for the CAVE build in `awips2-builds/cave/build/features.txt`:

    cat awips2-builds/cave/build/features.txt
    com.raytheon.uf.common.base.feature
    com.raytheon.uf.viz.dataplugin.obs.feature
    ...



    <target name="wa-build" depends="main-build" description="Builds work assignment specific features after the main build">
    	<if>
    		<available file="${basedir}/features.txt" type="file" />
    		<then>
    			<loadfile property="wa.features"
    				srcfile="${basedir}/features.txt" />
    			<for param="line" list="${wa.features}" 
    			delimiter="${line.separator}">
            		<sequential>
                		<antcall target="build">
                    		<param name="feature" value="@{line}" />
                		</antcall>
            		</sequential>    				
    			</for>
    		</then>
    	</if>

    	<antcall target="wa-cleanup" />
		<antcall target="clean" />
    </target>
    
