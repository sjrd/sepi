{*
  D�finit les constantes g�n�rales de Sepi
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
  sSepiAuthor = 'L''�quipe Sepi';
  sSepiAuthorEMail = 'sjrd@redaction-developpez.com';
  sSepiWebSite = 'http://sjrd.developpez.com/sepi/';
  sSepiCopyright = 'Sepi v%d.%d - Copyright � 2005-2006 SJRDoeraene';
  sSepiAbout = '� propos de Sepi';

  sSepiInstanceAlreadyExists = 'Seule une instance de TSepi peut �tre cr��e';
  sSepiDifferentVersion =
    'Versions majeures diff�rentes : incompatibilit� de format';
  sSepiUnexistingFile = 'Le fichier sp�cifi� n''existe pas';

implementation

end.

