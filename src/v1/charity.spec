
Summary: Charity language interpreter

Name: charity

Version: 1.99

Release: 1

Group: Development/Languages

Source: charity-1.99.tar.gz

License: See http://www.cpsc.ucalgary.ca/~charity

%description
The Charity language interpreter.  Charity is a categorical programming
language.

%prep

%setup

%build
./configure
make

%install
echo "RPM_BUILD_ROOT is $RPM_BUILD_ROOT"
install -s charity $RPM_BUILD_ROOT/usr/bin/
install -d $RPM_BUILD_ROOT/usr/share/charity
install -m 644 PRELUDE.ch $RPM_BUILD_ROOT/usr/share/charity/

%files
/usr/bin/charity
%dir /usr/share/charity
/usr/share/charity/PRELUDE.ch
