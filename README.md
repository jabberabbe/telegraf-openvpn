# telegraf-openvpn

[![GitHub CI](https://github.com/jabberabbe/telegraf-openvpn/workflows/CI/badge.svg)](https://github.com/jabberabbe/telegraf-openvpn/actions)
[![MIT license](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)

This script emits OpenVPN server statistics such as connected clients and total
sent/recvd bytes in InfluxDB data format. It is meant to be used with the
`execd` Telegraf plugin. It queries the server process via the management
socket.

