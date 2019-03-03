---
title: systemd 자주 사용하는 설정 옵션
author: 주형
tags: systemd, daemon, config
---

## 예시


```
[Unit]
Description=Test daemon

[Service]
ExecStart=/usr/bin/env node /home/jh/my-tiny-js.js

[Install]
WantedBy=default.target
```




## 참고자료

https://access.redhat.com/documentation/en-us/red_hat_enterprise_linux/7/html/system_administrators_guide/sect-managing_services_with_systemd-unit_files
