---
title: systemd에 유저 권한으로 서비스 추가하기
author: 주형
tags: systemd, daemon
---

systemd를 사용하여 유저별로 서비스를 추가할 수 있다. 설정에 따라 유저의 로그인할 때 실행되게 하거나 서버가 시작할 때 시작되도록 정할 수 있다.

local에 unit 추가하기
----------------------

시스템 전역 서비스는 `/etc/systemd/user/`에, 유저의 서비스는 `~/.config/systemd/user/` 작성한다. 

예시 unit 파일
--------------

```
[Unit]
Description=Test daemon

[Service]
ExecStart=/usr/bin/env node /home/jh/my-tiny-js.js

[Install]
WantedBy=default.target
```

systemd가 unit파일을 읽게 만들기
---------------------------------

유닛 파일이 변경되면 `daemon-reload`를 한 번 씩 실행해 주어야 한다.

`systemctl --user daemon-reload`

자동 실행하게 만들기
-------------------

enable된 유닛들만이 자동으로 실행된다. 자동 실행을 끄고 싶으면 `disable` 명령을 통해 끌 수 있다.

`systemctl --user enable myunit.service`

직접 unit을 실행시키기
----------------------

`systemctl --user start myunit.service`


기타 systemd 명령들
---------------------------------

멈추기:

`systemctl --user stop myunit.service`

현재 상태 확인:

`systemctl --user status myunit.service`


서비스가 항상 실행되도록 하기
----------------------------

`loginctl`을 사용하여 서비스가 서버가 켜졌을 때 항상 시작되게 만들 수 있다.

`loginctl enable-linger username`

로그 보기
----------

`journalctl`를 사용하여 내가 생성한 서비스의 로그를 확인할 수 있다.

`journalctl --user-unit myunit.service`


참고자료
--------

* [https://wiki.archlinux.org/index.php/Systemd/User](https://wiki.archlinux.org/index.php/Systemd/User)
* [https://nodesource.com/blog/running-your-node-js-app-with-systemd-part-1/](https://nodesource.com/blog/running-your-node-js-app-with-systemd-part-1/)