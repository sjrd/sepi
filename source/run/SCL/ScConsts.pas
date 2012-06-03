{-------------------------------------------------------------------------------
Sepi - Object-oriented script engine for Delphi
Copyright (C) 2006-2009  S�bastien Doeraene
All Rights Reserved

This file is part of Sepi.

Sepi is free software: you can redistribute it and/or modify it under the terms
of the GNU General Public License as published by the Free Software Foundation,
either version 3 of the License, or (at your option) any later version.

Sepi is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE.  See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with
Sepi.  If not, see <http://www.gnu.org/licenses/>.

Linking this library statically or dynamically with other modules is making a
combined work based on this library.  Thus, the terms and conditions of the GNU
General Public License cover the whole combination.

As a special exception, the copyright holders of this library give you
permission to link this library with independent modules to produce an
executable, regardless of the license terms of these independent modules, and
to copy and distribute the resulting executable under terms of your choice,
provided that you also meet, for each linked independent module, the terms and
conditions of the license of that module.  An independent module is a module
which is not derived from or based on this library.  If you modify this
library, you may extend this exception to your version of the library, but you
are not obligated to do so.  If you do not wish to do so, delete this exception
statement from your version.
-------------------------------------------------------------------------------}

{*
  D�finit les diff�rentes constantes de la SCL
  @author sjrd
  @version 1.0
*}
unit ScConsts;
{$i ..\..\source\Sepi.inc}
interface

resourcestring
  sScLanguage = 'Fran�ais (Belgique)';

  // Common consts
  sScYes = 'Oui';
  sScNo = 'Non';
  sScColon = ' :';
  sScSemiColon = ' ;';
  sScAbout = '� propos';
  sScVersion = 'Version';
  sScAuthor = 'Auteur';
  sScWrongPassword = 'Mot de passe incorrect';

  // Months consts
  sJanuary = 'janvier';
  sFebruary = 'f�vrier';
  sMarch = 'mars';
  sApril = 'avril';
  sMay = 'mai';
  sJune = 'juin';
  sJuly = 'juillet';
  sAugust = 'ao�t';
  sSeptember = 'septembre';
  sOctober = 'octobre';
  sNovember = 'novembre';
  sDecember = 'd�cembre';

  // Lists errors
  sScNotInMustPairsOfChars = 'NotIn doit contenir des paires de caract�res';
  sScDelimMustDifferentThanNotIn =
    'Delim et NotIn ne peuvent contenir un m�me caract�re';
  sScIndexOutOfRange = 'Index liste hors bornes (%d)';
  sScWrongIntSize = 'Taille d''entier incorrecte : %d';
  sScWrongLongWord =
    '''%s'' n''est pas une valeur enti�re non sign�e correcte';
  sScListIsNotEmpty = 'La liste n''est pas vide';

  // Extra errors
  sScWrongBase = 'La base %d est incorrecte';
  sScWrongInteger = 'Impossible de convertir "%s" en entier';
  sScWrongFloat = 'Impossible de convertir "%s" en valeur � virgule flottante';
  sScWrongString = 'Impossible de convertir "%s" en cha�ne de caract�res';
  sScWrongChar = 'Impossible de convertir "%s" en caract�re';
  sScWrongCharSet = 'Impossible de convertir "%s" en ensemble de caract�res';
  sScSetOf = 'ensemble de %s';
  sScWrongEnumSet = 'Impossible de convertir "%s" en %s';

  // Interfaces errors
  SAlreadyHasController = 'Cet objet a d�j� un contr�leur';
  SHasNoController = 'Cet objet n''a pas de contr�leur';

  // XML errors
  SCantLoadXMLDocument = 'Erreur lors du chargement du document XML';

  // Maths errors
  sScErrorMaths = 'Erreur de math�matiques :'+#10;
  sScErrorDegree = 'Erreur avec le degr� %s';
  sScErrorNotDegreeZero = '%s n''est pas de degr� 0';
  sScErrorInteger = 'Erreur d''entier avec le nombre %f';
  sScErrorNotInteger = '%f n''est pas un nombre entier';
  sScErrorNatural = 'Erreur de naturel avec le nombre %d';
  sScErrorNotNatural = '%d n''est pas un nombre naturel';
  sScErrorDivision = 'Impossible de diviser %s par %s';
  sScErrorDivByZero = 'Division par 0 de %s impossible';
  sSjrdErrorAbortDivision = 'La division de %s par %s semble impossible';
  sScErrorRoot = 'Impossible de calculer la racine %s de %s';
  sScErrorEval = 'Erreur lors de l''�valuation de l''expression %s :'+#10;
  sScErrorWrongExpression = 'Expression erron�e';
  sScErrorWrongCharacter = 'Caract�re non autoris� : %s';
  sScErrorOperation = 'Erreur avec l''op�rateur %s';
  sScErrorOpNotExists = 'L''op�rateur %s n''existe pas';
  sScErrorOpIsNotBinary = 'L''op�rateur %s n''est pas un op�rateur binaire';
  sScErrorOpIsNotUnary = 'L''op�rateur %s n''est pas un op�rateur binaire';
  sScErrorOpRequestsDegreeZero =
    'L''op�rateur %s requiert un monome de degr� 0 (%s est de degr� %d)';
  sScErrorOpRequestsInteger =
    'L''op�rateur %s requiert un entier (%f n''est pas un entier)';
  sScErrorOpRequestsNatural =
    'L''op�rateur %s requiert un naturel (%d n''est pas un naturel)';
  sScErrorOpRequestsCorrectIndex =
    'L''op�rateur %s requiert un index de liste correct (%d n''est pas valable)';
  sScErrorBrackets = 'Erreur de parenth�ses';
  sScErrorTooManyBrackets = '%d parenth�ses %s en trop';

  // Command line errors
  SDuplicateLongOption = 'Nom d''option %s dupliqu�';
  SDuplicateShortOption = 'Version courte d''option %s dupliqu�e';
  SUnknownLongOption = 'Option longue %s inconnue';
  SUnknownShortOption = 'Option courte %s inconnue';
  SInvalidTypeInfo = 'RTTI invalides';
  SArgumentRequired = 'L''option %s requiert un param�tre';
  SArgumentForbidden = 'L''option %s ne prend pas de param�tre';
  SNonOptionParamForbidden = 'Tous les param�tres doivent �tre des options';
  SSingleOptionFoundMultipleTimes =
    'L''option unique %s a �t� trouv�e plusieurs fois';
  SRequiredOptionNotFound = 'L''option requise %s n''a pas �t� fournie';
  SOptionValueOutOfRange =
    'La valeur de l''option %s doit �tre comprise dans les bornes %d � %d';
  SInvalidValue = 'Valeur ''%s'' invalide pour l''option %s';

implementation

end.

