{*
  Définit les constantes générales de Sepi
  @author sjrd
  @version 1.0
*}
unit SepiConsts;

interface

const
  SepiMajVersion = 1; /// Version majeure de Sepi
  SepiMinVersion = 0; /// Version mineure de Sepi

  /// Dossier racine de Sepi dans la base de registre
  regSepiBase = '\Software\SJRDoeraene\Sepi\'; {don't localize}

resourcestring
  sSepiName = 'Projet Sepi';
  sSepiAuthor = 'L''équipe Sepi';
  sSepiAuthorEMail = 'sjrd@redaction-developpez.com';
  sSepiWebSite = 'http://sjrd.developpez.com/sepi/';
  sSepiCopyright = 'Sepi v%d.%d - Copyright © 2005-2006 SJRDoeraene';
  sSepiAbout = 'À propos de Sepi';

  sSepiInstanceAlreadyExists = 'Seule une instance de TSepi peut être créée';
  sSepiDifferentVersion =
    'Versions majeures différentes : incompatibilité de format';
  sSepiUnexistingFile = 'Le fichier spécifié n''existe pas';

implementation

end.

