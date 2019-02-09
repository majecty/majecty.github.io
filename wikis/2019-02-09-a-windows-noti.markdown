---
title: Electron과 Windows Notification
author: 주형
tags: electron, windows, notification
---

## tldr;

Electron에서 Windows Notification을 쓰고 싶으면, 빌드한 실행파일을 시작화면에 고정시킨 뒤 프로그램 초기화할 때 `app.setAppUserModelId(process.execPath)` 를 실행시켜주면 된다. 그 이후부터 Electron의 Notification API를 사용할 수 있다. 위 과정은 일반 사용자가 하기엔 번거로운 작업이므로, 프로덕션으로 배포할 땐 설치프로그램을 제작해야한다.

## UWP와 Notification

UWP는 Universal Windows Platform의 약자로, UWP app은 Windwos 10이 동작하는 데스크탑, 엑스박스, 모일일 등에서 실행가능하며, MicroSoft Store에서 판매할 수 있다. 예전의 윈도우즈 개발환경에 모바일 마켓의 특징이 결합된 환경이다.

Windows 10에서 브라우저나 메일 앱 등을 사용할 때 볼 수 있는 Notification은 이 UWP에서 제공해주는 기능이다. 따라서 UWP 환경에서 개발한 앱은 쉽게 Notification을 사용할 수 있으나, 예전 개환경인 Windows API나 WPF를 쓰는 경우 조금은 복잡한 설정과정을 쳐쳐서 Notification을 쓸 수 있다.

## 시작화면에 고정

Windows의 Notification은 시작화면이나, All Programs에 바로가기가 등록된 앱들만이 Notification을 사용할 수 있다. 이 과정을 거치지 않았다면, Notification API를 호출하더라도 아무 일도 일어나지 않는다.

## Application User Model ID (AppUserModelId)

하나의 앱을 인식할 수 있게 하는 아이디이다. 같은 실행파일이더라도 유저가 보기에 서로 다른 어플리케이션처럼 동작한다면 다른 ID를 써야하며, 서로 다른 프로그램이더라도 유저에게는 한 어플리케이션처럼 보인다면 같은 ID를 써야한다. 이 ID는 All Programs나 시작화면에 등록한 바로가기에서 설정하거나, 실행중에 설정할 수 있다. 실행중에 설정한다면, Windows UI 코드를 실행하기 전에 등록해야한다. Electron에서 Notification을 띄우기 전에 AppUserModelId를 설정한다면 Notification이 잘 동작하지만, Notification을 한번이라도 먼저 호출한 뒤, AppUserModelId를 설정한다면, Notification이 동작하지 않는다.

## 개발중에 Notification 테스트하기

개발중이라면 Electron앱을 실행할 때 node_modules/electron/dist/electron.exe 파일이 실행된다. 이 파일을 탐색기에서 오른쪽 클릭한 뒤, 시작화면에 고정을 선택해야 한다.

## 참고자료

* [Electron Notifaction 튜토리얼](https://electronjs.org/docs/tutorial/notifications)
* [UWP 가이드](https://docs.microsoft.com/ko-kr/windows/uwp/get-started/universal-application-platform-guide)
* [AppUserModelI로 Toast Notification 켜기](https://docs.microsoft.com/ko-kr/windows/desktop/shell/enable-desktop-toast-with-appusermodelid)
* [Application User Model Ids](https://docs.microsoft.com/ko-kr/windows/desktop/shell/appids)
