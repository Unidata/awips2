Run deploy-install.xml to execute the new groovy-based deployment mechanism.
The mechanism will need to remain ant-wrapped until the Groovy plugins are added
to the uframe Eclipse distribution.

One argument is required to run the script: -Dworkspace_loc=${workspace_loc}

Note: ENTER THE ARGUMENT EXACTLY AS IT IS WRITTEN ABOVE. ${workspace_loc} is
an Eclipse variable that references the actual location of the workspace.

deploy-install will deploy any EDEX features it finds in the workspace excluding:
com.raytheon.edex.wa.feature and com.raytheon.edex.feature.uframe

Currently, an EDEX feature is defined as a feature project that includes edex in
the name. Feature deployment options will most likely be more customizable when
the pure groovy version of this deployment can be used.