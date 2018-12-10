open! Base
open Mtproto_transport
open Types

module Types = Types

module MakeMTProtoV2Client (T: MTProtoTransport): MTProtoClient
