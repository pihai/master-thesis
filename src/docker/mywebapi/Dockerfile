FROM microsoft/dotnet:1.1.1-runtime
COPY published app
WORKDIR app
ENV ASPNETCORE_URLS http://+:80
EXPOSE 80
ENTRYPOINT [ "dotnet", "mywebapi.dll" ]