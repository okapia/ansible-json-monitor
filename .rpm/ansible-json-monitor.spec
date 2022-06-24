%define __spec_install_post %{nil}
%define __os_install_post %{_dbpath}/brp-compress
%define debug_package %{nil}

Name: ansible-json-monitor
Summary: Simple ansible monitoring using json output
Version: @@VERSION@@
Release: 1
License: MIT or ASL 2.0
Group: Applications/System
Source0: %{name}-%{version}.tar.gz
URL: https://github.com/okapia/ansible-json-monitor

BuildRoot: %{_tmppath}/%{name}-%{version}-%{release}-root

%description
%{summary}

%prep
%setup -q

%install
rm -rf %{buildroot}
mkdir -p %{buildroot}/etc
cp -a * %{buildroot}
echo 'path = "%{_localstatedir}/log/ansible"' > %{buildroot}%{_sysconfdir}/ansible-json-monitor.conf
mkdir -m 0755 -p %{buildroot}%{_datadir}/zsh/site-functions
mkdir -m 0755 -p %{buildroot}%{_mandir}/man1
cp ../../../../../complete/_ansible-json-monitor %{buildroot}%{_datadir}/zsh/site-functions
chmod 644 %{buildroot}%{_datadir}/zsh/site-functions/_ansible-json-monitor
cp ../../../../../doc/ajmon.1 %{buildroot}%{_mandir}/man1
chmod 644 %{buildroot}%{_mandir}/man1/ajmon.1

%clean
rm -rf %{buildroot}

%files
%defattr(-,root,root,-)
%{_bindir}/*
%{_mandir}/
%{_datadir}/
%config(noreplace) %{_sysconfdir}/ansible-json-monitor.conf
