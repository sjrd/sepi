{*
  Définit les différentes constantes de la SCL
  @author sjrd
  @version 1.0
*}
unit ScConsts;

interface

resourcestring
  sScLanguage = 'Français (Belgique)';

// Common consts
  sScYes = 'Oui';
  sScNo = 'Non';
  sScColon = ' :';
  sScSemiColon = ' ;';
  sScAbout = 'À propos';
  sScVersion = 'Version';
  sScAuthor = 'Auteur';
  sScWrongPassword = 'Mot de passe incorrect';

// Months consts
  sJanuary = 'janvier';
  sFebruary = 'février';
  sMarch = 'mars';
  sApril = 'avril';
  sMay = 'mai';
  sJune = 'juin';
  sJuly = 'juillet';
  sAugust = 'août';
  sSeptember = 'septembre';
  sOctober = 'octobre';
  sNovember = 'novembre';
  sDecember = 'décembre';

// Lists errors
  sScNotInMustPairsOfChars = 'NotIn doit contenir des paires de caractères';
  sScDelimMustDifferentThanNotIn = 'Delim et NotIn ne peuvent contenir un même caractère';
  sScIndexOutOfRange = 'Index liste hors bornes (%d)';
  sScWrongIntSize = 'Taille d''entier incorrecte : %d';
  sScWrongLongWord = '''%s'' n''est pas une valeur entière non signée correcte';

// Extra errors
  sScWrongBase = 'La base %d est incorrecte';
  sScWrongInteger = 'Impossible de convertir "%s" en entier';
  sScWrongFloat = 'Impossible de convertir "%s" en valeur à virgule flottante';
  sScWrongString = 'Impossible de convertir "%s" en chaîne de caractères';
  sSjrdWrongChar = 'Impossible de convertir "%s" en caractère';
  sScWrongCharSet = 'Impossible de convertir "%s" en ensemble de caractères';
  sScSetOf = 'ensemble de %s';
  sScWrongEnumSet = 'Impossible de convertir "%s" en %s';

// Maths errors
  sScErrorMaths = 'Erreur de mathématiques :'+#10;
  sScErrorDegree = 'Erreur avec le degré %s';
  sScErrorNotDegreeZero = '%s n''est pas de degré 0';
  sScErrorInteger = 'Erreur d''entier avec le nombre %f';
  sScErrorNotInteger = '%f n''est pas un nombre entier';
  sScErrorNatural = 'Erreur de naturel avec le nombre %d';
  sScErrorNotNatural = '%d n''est pas un nombre naturel';
  sScErrorDivision = 'Impossible de diviser %s par %s';
  sScErrorDivByZero = 'Division par 0 de %s impossible';
  sSjrdErrorAbortDivision = 'La division de %s par %s semble impossible';
  sScErrorRoot = 'Impossible de calculer la racine %s de %s';
  sScErrorEval = 'Erreur lors de l''évaluation de l''expression %s :'+#10;
  sScErrorWrongExpression = 'Expression erronée';
  sScErrorWrongCharacter = 'Caractère non autorisé : %s';
  sScErrorOperation = 'Erreur avec l''opérateur %s';
  sScErrorOpNotExists = 'L''opérateur %s n''existe pas';
  sScErrorOpIsNotBinary = 'L''opérateur %s n''est pas un opérateur binaire';
  sScErrorOpIsNotUnary = 'L''opérateur %s n''est pas un opérateur binaire';
  sScErrorOpRequestsDegreeZero = 'L''opérateur %s requiert un monome de degré 0 (%s est de degré %d)';
  sScErrorOpRequestsInteger = 'L''opérateur %s requiert un entier (%f n''est pas un entier)';
  sScErrorOpRequestsNatural = 'L''opérateur %s requiert un naturel (%d n''est pas un naturel)';
  sScErrorOpRequestsCorrectIndex = 'L''opérateur %s requiert un index de liste correct (%d n''est pas valable)';
  sScErrorBrackets = 'Erreur de parenthèses';
  sScErrorTooManyBrackets = '%d parenthèses %s en trop';

implementation

end.
