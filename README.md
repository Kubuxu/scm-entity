### scm-entity
Entity is scheme based IRC bot.

#### Using
You have to install chicken and following modules: `sandbox irc symbol-utils`.

##### IPv6
IRC module does not support IPv6, for connecting to IPv6 servers use socat.
```
socat TCP4-LISTEN:6667,fork TCP:irc.fc00.io:6667
```

### License
```
Copyright © 2016 Jakub Sztandera <k.sztandera@protonmail.ch>

This work is free. You can redistribute it and/or modify it under the
terms of the Do What The Fuck You Want To But It's Not My Fault, Version 1
as published by Ben McGinnes. See the LICENSE file for more details.
```

