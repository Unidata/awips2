FROM centos:6
ENV VERSION 18.1.1
ENV RELEASE 1
MAINTAINER Tiffany Meyer <tiffanym@ucar.edu>
USER root

RUN yum update yum -y
RUN groupadd fxalpha && useradd -G fxalpha awips
RUN yum install epel-release wget -y
RUN yum clean all -y
RUN wget -O /etc/yum.repos.d/awips2.repo https://www.unidata.ucar.edu/software/awips2/doc/el6-dev.repo
COPY iptables /etc/sysconfig/iptables
COPY install-ade-server.sh /root/
RUN /root/install-ade-server.sh

ENTRYPOINT ["/bin/bash"]
