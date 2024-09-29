unit Manhuard.Strings;

{$mode ObjFPC}{$H+}

interface

resourcestring
  APP_NAME = 'Manhuard';
  APP_DESCRIPTION = 'An opensource software for downloading and managing manga books, and it can also be used as a manga viewer.';
  TEXT_ADD_MANGA_HELP = 'Currently supported manga websites are:' + LineEnding +
    '    - Manhuagui (https://www.manhuagui.com)' + LineEnding + LineEnding +
    'In some regions, you know, the proxy is necessary to access these websites.';

  MSG_CONFIG_SAVE_ERROR = 'Failed to save config file: "%s".';
  MSG_FAILED_TO_READ_VOLUME = 'Failed to read volume by path "%s"';
  MSG_FAILED_TO_READ_PAGE = 'Failed to read page by path "%s"';

const
  INTERNAL_NAME = 'manhuard';
  URL_SOURCE = 'https://github.com/tabris17/manhuard';
  URL_DOCUMENT = 'https://github.com/tabris17/manhuard/wiki';
  DEVELOPER = 'Fournoas';

implementation

end.

