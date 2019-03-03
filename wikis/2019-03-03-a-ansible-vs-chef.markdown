---
title: ansible과 chef 비교
author: 주형
tags: ansible, chef, devops
---

1. 공통점
---------

서버의 세팅을 미리 설정해놓고 자동화할 수 있음. Shell 스크립트에서 하기 힘든 모듈화를 할 수 있으며, 여러 서버를 손쉽게 세팅할 수 있음.

2. Ansible
-----------

Yaml 파일로 서버설정들을 관리하며, 서버 설정외에도 다양한 작업의 자동화를 목적으로 함.

### Adhoc

Adhoc이란 특정 명령을 여러 서버에서 실행시키는 방식이다.

### PlayBook

설정을 한 곳에 모아 모듈화 하는 방식이다.

3. Puppet
----------

Puppet은 Ansible에 비하여 서버의 세팅에 좀 더 초점이 맞춰져있다. Client와 Server로 구분되어있으며, Server 노드에 모든 설정들이 저장된다. 각 Client들은 Server로부터 설정 파일을 받아서 자신의 머신에 실행한다.


참고자료
--------

* [How ansible works(ansible.com)](https://www.ansible.com/overview/how-ansible-works)
* [Chef overview(docs.chef.io)](https://docs.chef.io/chef_overview.html)
