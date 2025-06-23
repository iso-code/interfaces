This package provides interfaces to hydrological data sources

Das Repository iso-code/interfaces stellt ein R-Paket bereit, das verschiedene Schnittstellen zu hydrologischen Datenquellen bietet. Es ermöglicht einfachen Zugriff auf aktuelle Rohdaten und geprüfte historische Daten. Das Paket kann direkt von GitHub installiert und über bereitgestellte Funktionen genutzt werden. Weitere Details und Beispiele sind in der Dokumentation zu finden. 

Installation
devtools::install_github("iso-code/interfaces")

library(interfaces)

# Datenquellen
Aktuell lassen sich Datenquellen wie folgt ermitteln
check_hub("type")

|type                |  content                   |
|--------------------|----------------------------|
|raw_nrw             |  Rohdaten der Pegelstände  |
|verified_runoff_nrw |  geprüfte Abflussdaten     |
|verified_level_nrw  |  geprüfte Wasserstandsdaten|




# Beispielaufruf einer Funktion
result <- get_hydrological_data("Basis_Url", parameters)
