{-------------------------------------------------------------------------------
Sepi - Object-oriented script engine for Delphi
Copyright (C) 2006-2007  Sébastien Doeraene
All Rights Reserved

This file is part of the SCL (Sepi Code Library), which is part of Sepi.

Sepi is free software: you can redistribute it and/or modify it under the terms
of the GNU General Public License as published by the Free Software Foundation,
either version 3 of the License, or (at your option) any later version.

Sepi is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE.  See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with
Sepi.  If not, see <http://www.gnu.org/licenses/>.
-------------------------------------------------------------------------------}

{*
  Constantes de l'importeur
  @author sjrd
  @version 1.0
*}
unit ImporterConsts;

interface

resourcestring
  // Context errors
  SBDSVersionNotInstalled = 'La version %s de BDS n''est pas installée';
  SBDSVendorVersionNotInstalled =
    'La version %1:s de BDS de %0:s n''est pas installée';

  // Options errors
  SOneOrMoreFileNamesRequired = 'Un argument ou plus requis';

  // Importer errors
  SCantFindProgram = 'Ne peut trouver le programme externe %s';
  SCantFindSourceFile = 'Le fichier source %s n''existe pas';
  SCantOpenSourceFile = 'Impossible de créer le fichier source %s';
  SCantOpenDestFile = 'Impossible de créer le fichier de sortie %s';
  SSepiInternalError = 'Erreur interne : %s';
  SCantCompileRCFile = 'Ne peut compiler le fichier de ressources %s';

const // don't localize
  PascalExt = '.pas';
  CompiledIntfExt = '.sci';
  RCExt = '.rc';
  MaxSizeBeforeLazyLoad = 100*1024;

  DefaultTemplatesDir = 'Templates\';
  DefaultCacheDir = 'Cache\';
  DefaultOutputDir = 'Output\';
  DefaultResourcesDir = 'Resources\';

implementation

end.

