apt-get update && \
    apt-get install -y wget

wget https://packages.microsoft.com/config/ubuntu/20.10/packages-microsoft-prod.deb -O packages-microsoft-prod.deb
dpkg -i packages-microsoft-prod.deb

apt-get install -y apt-transport-https && \ 
    apt-get update && \  
    apt-get install -y dotnet-sdk-5.0